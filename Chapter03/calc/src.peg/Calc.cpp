#include "llvm/IR/IRBuilder.h"
#include "peglib.h"

//-----------------------------------------------------------------------------
// Parser
//-----------------------------------------------------------------------------

std::shared_ptr<peg::Ast> parse(const std::string_view sv) {
  peg::parser parser(R"(
    calc        <- decl expr
    decl        <- ('with' ident (',' ident)* ':')?  { no_ast_opt }
    expr        <- term (term_op term)*
    term        <- factor (factor_op factor)*
    factor      <- ident / number / '(' expr ')'
    term_op     <- < [+-] >
    factor_op   <- < [*/] >
    ident       <- < [a-zA-Z]+ >
    number      <- < [0-9]+ >
    %whitespace <- [ \t\n]*
  )");

  parser.enable_ast<peg::Ast>();

  std::shared_ptr<peg::Ast> ast;
  if (parser.parse(sv, ast, "[commend line]")) {
    ast = parser.optimize_ast(ast);
  }
  return ast;
}

//-----------------------------------------------------------------------------
// Semantic Analyzer
//-----------------------------------------------------------------------------

class DeclCheck {
 public:
  static bool check(const std::shared_ptr<peg::Ast> ast) {
    DeclCheck vis;
    vis.visit(ast);
    return vis.ok_;
  }

 private:
  std::set<std::string_view> scope_;
  bool ok_ = true;

  void error(const std::shared_ptr<peg::Ast> node, const std::string &msg) {
    std::cerr << node->path << ":" << node->line << ":" << node->column << ": "
              << msg << std::endl;
    ok_ = false;
  }

  void visit(const std::shared_ptr<peg::Ast> ast) {
    using namespace peg::udl;

    switch (ast->tag) {
      case "decl"_: {
        const auto &nodes = ast->nodes;
        for (auto i = 0u; i < nodes.size(); i += 1) {
          auto ident = nodes[i]->token;
          if (scope_.count(ident)) {
            error(nodes[i],
                  "'" + std::string(ident) + "' is already defined...");
          } else {
            scope_.emplace(ident);
          }
        }
        break;
      }
      case "ident"_: {
        auto ident = ast->token;
        if (!scope_.count(ident)) {
          error(ast, "undefined variable '" + std::string(ident) + "'...");
        }
        break;
      }
      case "expr"_:
      case "term"_: {
        const auto &nodes = ast->nodes;
        for (auto i = 0u; i < nodes.size(); i += 2) {
          visit(nodes[i]);
        }
        break;
      }
      default: {
        for (auto node : ast->nodes) {
          visit(node);
        }
        break;
      }
    }
  }
};

//-----------------------------------------------------------------------------
// Code Generater
//-----------------------------------------------------------------------------

using namespace llvm;

class CodeGen {
 public:
  static void compile(const std::shared_ptr<peg::Ast> ast) {
    LLVMContext Ctx;
    auto M = new Module("calc.expr", Ctx);
    CodeGen vis(M);
    vis.run(ast);
    M->print(outs(), nullptr);
  }

 private:
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int8PtrTy;
  Type *Int8PtrPtrTy;
  Constant *Int32Zero;

  Value *V;
  std::map<std::string_view, Value *> nameMap;

  CodeGen(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int8PtrTy = Type::getInt8PtrTy(M->getContext());
    Int8PtrPtrTy = Int8PtrTy->getPointerTo();
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(const std::shared_ptr<peg::Ast> ast) {
    auto MainFty = FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
    auto MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    auto BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);

    visit(ast);

    auto CalcWriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    auto CalcWriteFn = Function::Create(
        CalcWriteFnTy, GlobalValue::ExternalLinkage, "calc_write", M);
    Builder.CreateCall(CalcWriteFnTy, CalcWriteFn, {V});

    Builder.CreateRet(Int32Zero);
  }

  void visit(const std::shared_ptr<peg::Ast> ast) {
    using namespace peg::udl;

    switch (ast->tag) {
      case "decl"_: {
        auto ReadFty = FunctionType::get(Int32Ty, {Int8PtrTy}, false);
        auto ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage,
                                       "calc_read", M);

        for (auto node : ast->nodes) {
          auto ident = node->token;

          // Create call to calc_read function.
          auto StrText = ConstantDataArray::getString(M->getContext(), ident);
          auto Str = new GlobalVariable(*M, StrText->getType(),
                                        /*isConstant=*/true,
                                        GlobalValue::PrivateLinkage, StrText,
                                        Twine(ident).concat(".str"));
          auto Ptr =
              Builder.CreateInBoundsGEP(Str, {Int32Zero, Int32Zero}, "ptr");
          auto Call = Builder.CreateCall(ReadFty, ReadFn, {Ptr});

          nameMap[ident] = Call;
        }
        break;
      }
      case "ident"_: {
        auto ident = ast->token;
        V = nameMap[ident];
        break;
      }
      case "number"_: {
        auto intval = ast->token_to_number<int>();
        V = ConstantInt::get(Int32Ty, intval, true);
        break;
      }
      case "expr"_:
      case "term"_: {
        const auto &nodes = ast->nodes;
        visit(nodes[0]);
        auto Left = V;

        for (auto i = 1u; i < nodes.size(); i += 2) {
          auto ope = nodes[i + 0]->token[0];
          visit(nodes[i + 1]);
          auto Right = V;

          switch (ope) {
            case '+':
              V = Builder.CreateNSWAdd(Left, Right);
              break;
            case '-':
              V = Builder.CreateNSWSub(Left, Right);
              break;
            case '*':
              V = Builder.CreateNSWMul(Left, Right);
              break;
            case '/': {
              V = Builder.CreateSDiv(Left, Right);
              break;
            }
          }
        }
        break;
      }
      default: {
        for (auto node : ast->nodes) {
          visit(node);
        }
        break;
      }
    }
  }
};

//-----------------------------------------------------------------------------
// Main
//-----------------------------------------------------------------------------

int main(int argc, const char **argv) {
  if (argc < 2 || std::string("--help") == argv[1]) {
    std::cerr << "usage: calc [formula]" << std::endl;
    return 1;
  }
  std::string source = argv[1];

  auto ast = parse(source);
  if (ast) {
    // std::cout << ast_to_s(ast);
    try {
      if (DeclCheck::check(ast)) {
        CodeGen::compile(ast);
      }
    } catch (const std::runtime_error &e) {
      std::cerr << e.what() << std::endl;
    }
    return 0;
  }

  std::cout << "syntax error..." << std::endl;
  return 0;
}
