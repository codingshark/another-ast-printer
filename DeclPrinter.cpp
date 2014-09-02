//===--- DeclPrinter.cpp - Printing implementation for Decl ASTs ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Decl::print method, which pretty prints the
// AST back out to C/Objective-C/C++/Objective-C++ code.
//
//===----------------------------------------------------------------------===//

#include "DeclPrinter.h"
#include "StmtPrinter.h"

/*
void Decl::print(raw_ostream &Out, unsigned Indentation,
                 bool PrintInstantiation) const {
  print(Out, getASTContext().getPrintingPolicy(), Indentation, PrintInstantiation);
}

void Decl::print(raw_ostream &Out, const another_printer::PrintingPolicy &Policy,
                 unsigned Indentation, bool PrintInstantiation) const {
  DeclPrinter Printer(Out, Policy, Indentation, PrintInstantiation);
  Printer.Visit(const_cast<Decl*>(this));
}

void Decl::printGroup(Decl** Begin, unsigned NumDecls,
                      raw_ostream &Out, const another_printer::PrintingPolicy &Policy,
                      unsigned Indentation) {
  if (NumDecls == 1) {
    (*Begin)->print(Out, Policy, Indentation);
    return;
  }

  Decl** End = Begin + NumDecls;
  TagDecl* TD = dyn_cast<TagDecl>(*Begin);
  if (TD)
    ++Begin;

  another_printer::PrintingPolicy SubPolicy(Policy);
  if (TD && TD->isCompleteDefinition()) {
    TD->print(Out, Policy, Indentation);
    Out << " ";
    SubPolicy.SuppressTag = true;
  }

  bool isFirst = true;
  for ( ; Begin != End; ++Begin) {
    if (isFirst) {
      SubPolicy.SuppressSpecifiers = false;
      isFirst = false;
    } else {
      if (!isFirst) Out << ", ";
      SubPolicy.SuppressSpecifiers = true;
    }

    (*Begin)->print(Out, SubPolicy, Indentation);
  }
}

LLVM_DUMP_METHOD void DeclContext::dumpDeclContext() const {
  // Get the translation unit
  const DeclContext *DC = this;
  while (!DC->isTranslationUnit())
    DC = DC->getParent();
  
  ASTContext &Ctx = cast<TranslationUnitDecl>(DC)->getASTContext();
  DeclPrinter Printer(llvm::errs(), Ctx.getPrintingPolicy(), 0);
  Printer.VisitDeclContext(const_cast<DeclContext *>(this), //Indent=
  false);
}*/

namespace another_printer{


static QualType GetBaseType(QualType T) {
  // FIXME: This should be on the Type class!
  QualType BaseType = T;
  while (!BaseType->isSpecifierType()) {
    if (isa<TypedefType>(BaseType))
      break;
    else if (const PointerType* PTy = BaseType->getAs<PointerType>())
      BaseType = PTy->getPointeeType();
    else if (const BlockPointerType *BPy = BaseType->getAs<BlockPointerType>())
      BaseType = BPy->getPointeeType();
    else if (const ArrayType* ATy = dyn_cast<ArrayType>(BaseType))
      BaseType = ATy->getElementType();
    else if (const FunctionType* FTy = BaseType->getAs<FunctionType>())
      BaseType = FTy->getReturnType();
    else if (const VectorType *VTy = BaseType->getAs<VectorType>())
      BaseType = VTy->getElementType();
    else if (const ReferenceType *RTy = BaseType->getAs<ReferenceType>())
      BaseType = RTy->getPointeeType();
    else
      llvm_unreachable("Unknown declarator!");
  }
  return BaseType;
}

static QualType getDeclType(Decl* D) {
  if (TypedefNameDecl* TDD = dyn_cast<TypedefNameDecl>(D))
    return TDD->getUnderlyingType();
  if (ValueDecl* VD = dyn_cast<ValueDecl>(D))
    return VD->getType();
  return QualType();
}


void DeclPrinter::PrintStmt(const Stmt* p_stmt,
					raw_ostream & an_out,
					PrinterHelper * p_helper,
					const another_printer::PrintingPolicy & a_policy,
					unsigned an_indentation)
{
	another_printer::StmtPrinter P(an_out, p_helper, a_policy, an_indentation);
	P.Visit(const_cast<Stmt*>(p_stmt));
}

raw_ostream& DeclPrinter::Indent(unsigned Indentation) {
  for (unsigned i = 0; i != Indentation; ++i)
    Out << "  ";
  return Out;
}

void DeclPrinter::prettyPrintAttributes(Decl *D) {
  if (Policy.PolishForDeclaration)
    return;
  
  if (D->hasAttrs()) {
    AttrVec &Attrs = D->getAttrs();
    for (AttrVec::const_iterator i=Attrs.begin(), e=Attrs.end(); i!=e; ++i) {
      Attr *A = *i;
      A->printPretty(Out, Policy);
    }
  }
}

void DeclPrinter::printDeclType(QualType T, StringRef DeclName, bool Pack) {
  // Normally, a PackExpansionType is written as T[3]... (for instance, as a
  // template argument), but if it is the type of a declaration, the ellipsis
  // is placed before the name being declared.
  if (auto *PET = T->getAs<PackExpansionType>()) {
    Pack = true;
    T = PET->getPattern();
  }
  T.print(Out, Policy, (Pack ? "..." : "") + DeclName);
}

void DeclPrinter::ProcessDeclGroup(SmallVectorImpl<Decl*>& Decls) {
  this->Indent();
  Decl::printGroup(Decls.data(), Decls.size(), Out, Policy, Indentation);
  Out << ";\n";
  Decls.clear();
}

void DeclPrinter::Print(AccessSpecifier AS) {
  switch(AS) {
  case AS_none:      llvm_unreachable("No access specifier!");
  case AS_public:    Out << "public"; break;
  case AS_protected: Out << "protected"; break;
  case AS_private:   Out << "private"; break;
  }
}

//----------------------------------------------------------------------------
// Common C declarations
//----------------------------------------------------------------------------

void DeclPrinter::VisitDeclContext(DeclContext *DC, bool Indent) {
  if (Policy.TerseOutput)
    return;

  if (Indent)
    Indentation += Policy.Indentation;

  SmallVector<Decl*, 2> Decls;
  for (DeclContext::decl_iterator D = DC->decls_begin(), DEnd = DC->decls_end();
       D != DEnd; ++D) {

    // Don't print ObjCIvarDecls, as they are printed when visiting the
    // containing ObjCInterfaceDecl.
    if (isa<ObjCIvarDecl>(*D))
      continue;

    // Skip over implicit declarations in pretty-printing mode.
    if (D->isImplicit())
      continue;

    // The next bits of code handles stuff like "struct {int x;} a,b"; we're
    // forced to merge the declarations because there's no other way to
    // refer to the struct in question.  This limited merging is safe without
    // a bunch of other checks because it only merges declarations directly
    // referring to the tag, not typedefs.
    //
    // Check whether the current declaration should be grouped with a previous
    // unnamed struct.
    QualType CurDeclType = getDeclType(*D);
    if (!Decls.empty() && !CurDeclType.isNull()) {
      QualType BaseType = GetBaseType(CurDeclType);
      if (!BaseType.isNull() && isa<ElaboratedType>(BaseType))
        BaseType = cast<ElaboratedType>(BaseType)->getNamedType();
      if (!BaseType.isNull() && isa<TagType>(BaseType) &&
          cast<TagType>(BaseType)->getDecl() == Decls[0]) {
        Decls.push_back(*D);
        continue;
      }
    }

    // If we have a merged group waiting to be handled, handle it now.
    if (!Decls.empty())
      ProcessDeclGroup(Decls);

    // If the current declaration is an unnamed tag type, save it
    // so we can merge it with the subsequent declaration(s) using it.
    if (isa<TagDecl>(*D) && !cast<TagDecl>(*D)->getIdentifier()) {
      Decls.push_back(*D);
      continue;
    }

    if (isa<AccessSpecDecl>(*D)) {
      Indentation -= Policy.Indentation;
      this->Indent();
      Print(D->getAccess());
      Out << ":\n";
      Indentation += Policy.Indentation;
      continue;
    }

    this->Indent();
    Visit(*D);

    // FIXME: Need to be able to tell the DeclPrinter when
    const char *Terminator = nullptr;
    if (isa<OMPThreadPrivateDecl>(*D))
      Terminator = nullptr;
    else if (isa<FunctionDecl>(*D) &&
             cast<FunctionDecl>(*D)->isThisDeclarationADefinition())
      Terminator = nullptr;
    else if (isa<ObjCMethodDecl>(*D) && cast<ObjCMethodDecl>(*D)->getBody())
      Terminator = nullptr;
    else if (isa<NamespaceDecl>(*D) || isa<LinkageSpecDecl>(*D) ||
             isa<ObjCImplementationDecl>(*D) ||
             isa<ObjCInterfaceDecl>(*D) ||
             isa<ObjCProtocolDecl>(*D) ||
             isa<ObjCCategoryImplDecl>(*D) ||
             isa<ObjCCategoryDecl>(*D))
      Terminator = nullptr;
    else if (isa<EnumConstantDecl>(*D)) {
      DeclContext::decl_iterator Next = D;
      ++Next;
      if (Next != DEnd)
        Terminator = ",";
    } else
      Terminator = ";";

    if (Terminator)
      Out << Terminator;
    Out << "\n";
  }

  if (!Decls.empty())
    ProcessDeclGroup(Decls);

  if (Indent)
    Indentation -= Policy.Indentation;
}

void DeclPrinter::VisitTranslationUnitDecl(TranslationUnitDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<TranslationUnitDecl>";
  }
  VisitDeclContext(D, false);
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</TranslationUnitDecl>";
  }
}

void DeclPrinter::VisitTypedefDecl(TypedefDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<TypedefDecl>";
  }
  if (!Policy.SuppressSpecifiers) {
    Out << "typedef ";
    
    if (D->isModulePrivate())
      Out << "__module_private__ ";
  }
  D->getTypeSourceInfo()->getType().print(Out, Policy, D->getName());
  prettyPrintAttributes(D);
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</TypedefDecl>";
  }
}

void DeclPrinter::VisitTypeAliasDecl(TypeAliasDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<TypeAliasDecl>";
  }
  Out << "using " << *D;
  prettyPrintAttributes(D);
  Out << " = " << D->getTypeSourceInfo()->getType().getAsString(Policy);
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</TypeAliasDecl>";
  }
}

void DeclPrinter::VisitEnumDecl(EnumDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<EnumDecl>";
  }
  if (!Policy.SuppressSpecifiers && D->isModulePrivate())
    Out << "__module_private__ ";
  Out << "enum ";
  if (D->isScoped()) {
    if (D->isScopedUsingClassTag())
      Out << "class ";
    else
      Out << "struct ";
  }
  Out << *D;

  if (D->isFixed())
    Out << " : " << D->getIntegerType().stream(Policy);

  if (D->isCompleteDefinition()) {
    Out << " {\n";
    VisitDeclContext(D);
    Indent() << "}";
  }
  prettyPrintAttributes(D);
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</EnumDecl>";
  }
}

void DeclPrinter::VisitRecordDecl(RecordDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<RecordDecl>";
  }
  if (!Policy.SuppressSpecifiers && D->isModulePrivate())
    Out << "__module_private__ ";
  Out << D->getKindName();
  if (D->getIdentifier())
    Out << ' ' << *D;

  if (D->isCompleteDefinition()) {
    Out << " {\n";
    VisitDeclContext(D);
    Indent() << "}";
  }
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</RecordDecl>";
  }
}

void DeclPrinter::VisitEnumConstantDecl(EnumConstantDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<EnumConstantDecl>";
  }
  Out << *D;
  if (Expr *Init = D->getInitExpr()) {
    Out << " = ";
//    Init->printPretty(Out, nullptr, Policy, Indentation);
    PrintStmt(Init, Out, nullptr, Policy, Indentation);
  }
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</EnumConstantDecl>";
  }
}

void DeclPrinter::VisitFunctionDecl(FunctionDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<FunctionDecl>";
  }
  CXXConstructorDecl *CDecl = dyn_cast<CXXConstructorDecl>(D);
  CXXConversionDecl *ConversionDecl = dyn_cast<CXXConversionDecl>(D);
  if (!Policy.SuppressSpecifiers) {
    switch (D->getStorageClass()) {
    case SC_None: break;
    case SC_Extern: Out << "extern "; break;
    case SC_Static: Out << "static "; break;
    case SC_PrivateExtern: Out << "__private_extern__ "; break;
    case SC_Auto: case SC_Register: case SC_OpenCLWorkGroupLocal:
      llvm_unreachable("invalid for functions");
    }

    if (D->isInlineSpecified())  Out << "inline ";
    if (D->isVirtualAsWritten()) Out << "virtual ";
    if (D->isModulePrivate())    Out << "__module_private__ ";
    if (D->isConstexpr() && !D->isExplicitlyDefaulted()) Out << "constexpr ";
    if ((CDecl && CDecl->isExplicitSpecified()) ||
        (ConversionDecl && ConversionDecl->isExplicit()))
      Out << "explicit ";
  }

  another_printer::PrintingPolicy SubPolicy(Policy);
  SubPolicy.SuppressSpecifiers = false;
  std::string Proto = D->getNameInfo().getAsString();

  QualType Ty = D->getType();
  while (const ParenType *PT = dyn_cast<ParenType>(Ty)) {
    Proto = '(' + Proto + ')';
    Ty = PT->getInnerType();
  }

  if (const FunctionType *AFT = Ty->getAs<FunctionType>()) {
    const FunctionProtoType *FT = nullptr;
    if (D->hasWrittenPrototype())
      FT = dyn_cast<FunctionProtoType>(AFT);

    Proto += "(";
    if (FT) {
      llvm::raw_string_ostream POut(Proto);
      another_printer::DeclPrinter ParamPrinter(POut, SubPolicy, Indentation);
      for (unsigned i = 0, e = D->getNumParams(); i != e; ++i) {
        if (i) POut << ", ";
        ParamPrinter.VisitParmVarDecl(D->getParamDecl(i));
      }

      if (FT->isVariadic()) {
        if (D->getNumParams()) POut << ", ";
        POut << "...";
      }
    } else if (D->doesThisDeclarationHaveABody() && !D->hasPrototype()) {
      for (unsigned i = 0, e = D->getNumParams(); i != e; ++i) {
        if (i)
          Proto += ", ";
        Proto += D->getParamDecl(i)->getNameAsString();
      }
    }

    Proto += ")";
    
    if (FT) {
      if (FT->isConst())
        Proto += " const";
      if (FT->isVolatile())
        Proto += " volatile";
      if (FT->isRestrict())
        Proto += " restrict";

      switch (FT->getRefQualifier()) {
      case RQ_None:
        break;
      case RQ_LValue:
        Proto += " &";
        break;
      case RQ_RValue:
        Proto += " &&";
        break;
      }
    }

    if (FT && FT->hasDynamicExceptionSpec()) {
      Proto += " throw(";
      if (FT->getExceptionSpecType() == EST_MSAny)
        Proto += "...";
      else 
        for (unsigned I = 0, N = FT->getNumExceptions(); I != N; ++I) {
          if (I)
            Proto += ", ";

          Proto += FT->getExceptionType(I).getAsString(SubPolicy);
        }
      Proto += ")";
    } else if (FT && isNoexceptExceptionSpec(FT->getExceptionSpecType())) {
      Proto += " noexcept";
      if (FT->getExceptionSpecType() == EST_ComputedNoexcept) {
        Proto += "(";
        llvm::raw_string_ostream EOut(Proto);
        //FT->getNoexceptExpr()->printPretty(EOut, nullptr, SubPolicy, Indentation);
        PrintStmt(FT->getNoexceptExpr(), EOut, nullptr, SubPolicy, Indentation);
        EOut.flush();
        Proto += EOut.str();
        Proto += ")";
      }
    }

    if (CDecl) {
      bool HasInitializerList = false;
      for (const auto *BMInitializer : CDecl->inits()) {
        if (BMInitializer->isInClassMemberInitializer())
          continue;

        if (!HasInitializerList) {
          Proto += " : ";
          Out << Proto;
          Proto.clear();
          HasInitializerList = true;
        } else
          Out << ", ";

        if (BMInitializer->isAnyMemberInitializer()) {
          FieldDecl *FD = BMInitializer->getAnyMember();
          Out << *FD;
        } else {
          Out << QualType(BMInitializer->getBaseClass(), 0).getAsString(Policy);
        }
        
        Out << "(";
        if (!BMInitializer->getInit()) {
          // Nothing to print
        } else {
          Expr *Init = BMInitializer->getInit();
          if (ExprWithCleanups *Tmp = dyn_cast<ExprWithCleanups>(Init))
            Init = Tmp->getSubExpr();
          
          Init = Init->IgnoreParens();

          Expr *SimpleInit = nullptr;
          Expr **Args = nullptr;
          unsigned NumArgs = 0;
          if (ParenListExpr *ParenList = dyn_cast<ParenListExpr>(Init)) {
            Args = ParenList->getExprs();
            NumArgs = ParenList->getNumExprs();
          } else if (CXXConstructExpr *Construct
                                        = dyn_cast<CXXConstructExpr>(Init)) {
            Args = Construct->getArgs();
            NumArgs = Construct->getNumArgs();
          } else
            SimpleInit = Init;
          
          if (SimpleInit)
//            SimpleInit->printPretty(Out, nullptr, Policy, Indentation);
            PrintStmt(SimpleInit, Out, nullptr, Policy, Indentation);
          else {
            for (unsigned I = 0; I != NumArgs; ++I) {
              assert(Args[I] != nullptr && "Expected non-null Expr");
              if (isa<CXXDefaultArgExpr>(Args[I]))
                break;
              
              if (I)
                Out << ", ";
//              Args[I]->printPretty(Out, nullptr, Policy, Indentation);
              PrintStmt(Args[I], Out, nullptr, Policy, Indentation);
            }
          }
        }
        Out << ")";
        if (BMInitializer->isPackExpansion())
          Out << "...";
      }
    } else if (!ConversionDecl && !isa<CXXDestructorDecl>(D)) {
      if (FT && FT->hasTrailingReturn()) {
        Out << "auto " << Proto << " -> ";
        Proto.clear();
      }
      AFT->getReturnType().print(Out, Policy, Proto);
      Proto.clear();
    }
    Out << Proto;
  } else {
    Ty.print(Out, Policy, Proto);
  }

  prettyPrintAttributes(D);

  if (D->isPure())
    Out << " = 0";
  else if (D->isDeletedAsWritten())
    Out << " = delete";
  else if (D->isExplicitlyDefaulted())
    Out << " = default";
  else if (D->doesThisDeclarationHaveABody() && !Policy.TerseOutput) {
    if (!D->hasPrototype() && D->getNumParams()) {
      // This is a K&R function definition, so we need to print the
      // parameters.
      Out << '\n';
      another_printer::DeclPrinter ParamPrinter(Out, SubPolicy, Indentation);
      Indentation += Policy.Indentation;
      for (unsigned i = 0, e = D->getNumParams(); i != e; ++i) {
        Indent();
        ParamPrinter.VisitParmVarDecl(D->getParamDecl(i));
        Out << ";\n";
      }
      Indentation -= Policy.Indentation;
    } else
      Out << ' ';

    if (D->getBody())
      //D->getBody()->printPretty(Out, nullptr, SubPolicy, Indentation);
      PrintStmt(D->getBody(), Out, nullptr, SubPolicy, Indentation);
    Out << '\n';
  }
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</FunctionDecl>";
  }
}

void DeclPrinter::VisitFriendDecl(FriendDecl *D) {
 if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<FriendDecl>";
  }
  if (TypeSourceInfo *TSI = D->getFriendType()) {
    unsigned NumTPLists = D->getFriendTypeNumTemplateParameterLists();
    for (unsigned i = 0; i < NumTPLists; ++i)
      PrintTemplateParameters(D->getFriendTypeTemplateParameterList(i));
    Out << "friend ";
    Out << " " << TSI->getType().getAsString(Policy);
  }
  else if (FunctionDecl *FD =
      dyn_cast<FunctionDecl>(D->getFriendDecl())) {
    Out << "friend ";
    VisitFunctionDecl(FD);
  }
  else if (FunctionTemplateDecl *FTD =
           dyn_cast<FunctionTemplateDecl>(D->getFriendDecl())) {
    Out << "friend ";
    VisitFunctionTemplateDecl(FTD);
  }
  else if (ClassTemplateDecl *CTD =
           dyn_cast<ClassTemplateDecl>(D->getFriendDecl())) {
    Out << "friend ";
    VisitRedeclarableTemplateDecl(CTD);
  }
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</FriendDecl>";
  }
}

void DeclPrinter::VisitFieldDecl(FieldDecl *D) {
 if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<FieldDecl>";
  }
  if (!Policy.SuppressSpecifiers && D->isMutable())
    Out << "mutable ";
  if (!Policy.SuppressSpecifiers && D->isModulePrivate())
    Out << "__module_private__ ";

  Out << D->getASTContext().getUnqualifiedObjCPointerType(D->getType()).
            stream(Policy, D->getName());

  if (D->isBitField()) {
    Out << " : ";
    //D->getBitWidth()->printPretty(Out, nullptr, Policy, Indentation);
    PrintStmt(D->getBitWidth(), Out, nullptr, Policy, Indentation);
  }

  Expr *Init = D->getInClassInitializer();
  if (!Policy.SuppressInitializers && Init) {
    if (D->getInClassInitStyle() == ICIS_ListInit)
      Out << " ";
    else
      Out << " = ";
//    Init->printPretty(Out, nullptr, Policy, Indentation);
    PrintStmt(Init, Out, nullptr, Policy, Indentation);
  }
  prettyPrintAttributes(D);
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</FieldDecl>";
  }
}

void DeclPrinter::VisitLabelDecl(LabelDecl *D) {
 if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<LabelDecl>";
  }
  Out << *D << ":";
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</LabelDecl>";
  }
}

void DeclPrinter::VisitVarDecl(VarDecl *D) {
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<VarDecl>";
  }
  
  if (!Policy.SuppressSpecifiers) {
    StorageClass SC = D->getStorageClass();
    if (SC != SC_None)
      Out << VarDecl::getStorageClassSpecifierString(SC) << " ";

    switch (D->getTSCSpec()) {
    case TSCS_unspecified:
      break;
    case TSCS___thread:
      Out << "__thread ";
      break;
    case TSCS__Thread_local:
      Out << "_Thread_local ";
      break;
    case TSCS_thread_local:
      Out << "thread_local ";
      break;
    }

    if (D->isModulePrivate())
      Out << "__module_private__ ";
  }

  QualType T = D->getTypeSourceInfo()
    ? D->getTypeSourceInfo()->getType()
    : D->getASTContext().getUnqualifiedObjCPointerType(D->getType());
  printDeclType(T, D->getName());
  Expr *Init = D->getInit();
  if (!Policy.SuppressInitializers && Init) {
    bool ImplicitInit = false;
    if (CXXConstructExpr *Construct =
            dyn_cast<CXXConstructExpr>(Init->IgnoreImplicit())) {
      if (D->getInitStyle() == VarDecl::CallInit &&
          !Construct->isListInitialization()) {
        ImplicitInit = Construct->getNumArgs() == 0 ||
          Construct->getArg(0)->isDefaultArgument();
      }
    }
    if (!ImplicitInit) {
      if ((D->getInitStyle() == VarDecl::CallInit) && !isa<ParenListExpr>(Init))
        Out << "(";
      else if (D->getInitStyle() == VarDecl::CInit) {
        Out << " = ";
      }
//      Init->printPretty(Out, nullptr, Policy, Indentation);
      PrintStmt(Init, Out, nullptr, Policy, Indentation);
      if ((D->getInitStyle() == VarDecl::CallInit) && !isa<ParenListExpr>(Init))
        Out << ")";
    }
  }
  prettyPrintAttributes(D);
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</VarDecl>";
  }
}

void DeclPrinter::VisitParmVarDecl(ParmVarDecl *D) {
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ParmVarDecl>";
  }
  VisitVarDecl(D);
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ParmVarDecl>";
  }
}

void DeclPrinter::VisitFileScopeAsmDecl(FileScopeAsmDecl *D) {
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<FileScopeAsmDecl>";
  }
  Out << "__asm (";
//  D->getAsmString()->printPretty(Out, nullptr, Policy, Indentation);
  PrintStmt(D->getAsmString(), Out, nullptr, Policy, Indentation);
  Out << ")";
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</FileScopeAsmDecl>";
  }
}

void DeclPrinter::VisitImportDecl(ImportDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ImportDecl>";
  }
  Out << "@import " << D->getImportedModule()->getFullModuleName()
      << ";\n";
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ImportDecl>";
  }
}

void DeclPrinter::VisitStaticAssertDecl(StaticAssertDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<StaticAssertDecl>";
  }
  Out << "static_assert(";
  //D->getAssertExpr()->printPretty(Out, nullptr, Policy, Indentation);
  PrintStmt(D->getAssertExpr(), Out, nullptr, Policy, Indentation);
  Out << ", ";
//  D->getMessage()->printPretty(Out, nullptr, Policy, Indentation);
  PrintStmt(D->getMessage(), Out, nullptr, Policy, Indentation);
  Out << ")";
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</StaticAssertDecl>";
  }
}

//----------------------------------------------------------------------------
// C++ declarations
//----------------------------------------------------------------------------
void DeclPrinter::VisitNamespaceDecl(NamespaceDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<NamespaceDecl>";
  }
  
  if (D->isInline())
    Out << "inline ";
  Out << "namespace " << *D << " {\n";
  VisitDeclContext(D);
  Indent() << "}";
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</NamespaceDecl>";
  }
}

void DeclPrinter::VisitUsingDirectiveDecl(UsingDirectiveDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<UsingDirectiveDecl>";
  }
  Out << "using namespace ";
  if (D->getQualifier())
    D->getQualifier()->print(Out, Policy);
  Out << *D->getNominatedNamespaceAsWritten();
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</UsingDirectiveDecl>";
  }
}

void DeclPrinter::VisitNamespaceAliasDecl(NamespaceAliasDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<NamespaceAliasDecl>";
  }
  Out << "namespace " << *D << " = ";
  if (D->getQualifier())
    D->getQualifier()->print(Out, Policy);
  Out << *D->getAliasedNamespace();
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</NamespaceAliasDecl>";
  }
}

void DeclPrinter::VisitEmptyDecl(EmptyDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<EmptyDecl>";
  }
  prettyPrintAttributes(D);
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</EmptyDecl>";
  }
}

void DeclPrinter::VisitCXXRecordDecl(CXXRecordDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<CXXRecordDecl>";
  }
  
  if (!Policy.SuppressSpecifiers && D->isModulePrivate())
    Out << "__module_private__ ";
  Out << D->getKindName();
  if (D->getIdentifier())
    Out << ' ' << *D;

  if (D->isCompleteDefinition()) {
    // Print the base classes
    if (D->getNumBases()) {
      Out << " : ";
      for (CXXRecordDecl::base_class_iterator Base = D->bases_begin(),
             BaseEnd = D->bases_end(); Base != BaseEnd; ++Base) {
        if (Base != D->bases_begin())
          Out << ", ";

        if (Base->isVirtual())
          Out << "virtual ";

        AccessSpecifier AS = Base->getAccessSpecifierAsWritten();
        if (AS != AS_none) {
          Print(AS);
          Out << " ";
        }
        Out << Base->getType().getAsString(Policy);

        if (Base->isPackExpansion())
          Out << "...";
      }
    }

    // Print the class definition
    // FIXME: Doesn't print access specifiers, e.g., "public:"
    Out << " {\n";
    VisitDeclContext(D);
    Indent() << "}";
  }
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</CXXRecordDecl>";
  }
}

void DeclPrinter::VisitLinkageSpecDecl(LinkageSpecDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<LinkageSpecDecl>";
  }
  const char *l;
  if (D->getLanguage() == LinkageSpecDecl::lang_c)
    l = "C";
  else {
    assert(D->getLanguage() == LinkageSpecDecl::lang_cxx &&
           "unknown language in linkage specification");
    l = "C++";
  }

  Out << "extern \"" << l << "\" ";
  if (D->hasBraces()) {
    Out << "{\n";
    VisitDeclContext(D);
    Indent() << "}";
  } else
    Visit(*D->decls_begin());
   if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</LinkageSpecDecl>";
  }
}

void DeclPrinter::PrintTemplateParameters(const TemplateParameterList *Params,
                                          const TemplateArgumentList *Args) {
  assert(Params);
  assert(!Args || Params->size() == Args->size());

  Out << "template <";

  for (unsigned i = 0, e = Params->size(); i != e; ++i) {
    if (i != 0)
      Out << ", ";

    const Decl *Param = Params->getParam(i);
    if (const TemplateTypeParmDecl *TTP =
          dyn_cast<TemplateTypeParmDecl>(Param)) {

      if (TTP->wasDeclaredWithTypename())
        Out << "typename ";
      else
        Out << "class ";

      if (TTP->isParameterPack())
        Out << "...";

      Out << *TTP;

      if (Args) {
        Out << " = ";
        Args->get(i).print(Policy, Out);
      } else if (TTP->hasDefaultArgument()) {
        Out << " = ";
        Out << TTP->getDefaultArgument().getAsString(Policy);
      };
    } else if (const NonTypeTemplateParmDecl *NTTP =
                 dyn_cast<NonTypeTemplateParmDecl>(Param)) {
      StringRef Name;
      if (IdentifierInfo *II = NTTP->getIdentifier())
        Name = II->getName();
      printDeclType(NTTP->getType(), Name, NTTP->isParameterPack());

      if (Args) {
        Out << " = ";
        Args->get(i).print(Policy, Out);
      } else if (NTTP->hasDefaultArgument()) {
        Out << " = ";
//        NTTP->getDefaultArgument()->printPretty(Out, nullptr, Policy,  Indentation);
       PrintStmt(NTTP->getDefaultArgument(), Out, nullptr, Policy, Indentation);
      }
    } else if (const TemplateTemplateParmDecl *TTPD =
                 dyn_cast<TemplateTemplateParmDecl>(Param)) {
      VisitTemplateDecl(TTPD);
      // FIXME: print the default argument, if present.
    }
  }

  Out << "> ";
}

void DeclPrinter::VisitTemplateDecl(const TemplateDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<TemplateDecl>";
  }
  
  PrintTemplateParameters(D->getTemplateParameters());

  if (const TemplateTemplateParmDecl *TTP =
        dyn_cast<TemplateTemplateParmDecl>(D)) {
    Out << "class ";
    if (TTP->isParameterPack())
      Out << "...";
    Out << D->getName();
  } else {
    Visit(D->getTemplatedDecl());
  }
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</TemplateDecl>";
  }
}

void DeclPrinter::VisitFunctionTemplateDecl(FunctionTemplateDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<FunctionTemplateDecl>";
  }
  
  if (PrintInstantiation) {
    TemplateParameterList *Params = D->getTemplateParameters();
    for (auto *I : D->specializations()) {
      PrintTemplateParameters(Params, I->getTemplateSpecializationArgs());
      Visit(I);
    }
  }

  VisitRedeclarableTemplateDecl(D);
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</FunctionTemplateDecl>";
  }
}

void DeclPrinter::VisitClassTemplateDecl(ClassTemplateDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ClassTemplateDecl>";
  }
  
  if (PrintInstantiation) {
    TemplateParameterList *Params = D->getTemplateParameters();
    for (auto *I : D->specializations()) {
      PrintTemplateParameters(Params, &I->getTemplateArgs());
      Visit(I);
      Out << '\n';
    }
  }

  VisitRedeclarableTemplateDecl(D);
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ClassTemplateDecl>";
  }
}

//----------------------------------------------------------------------------
// Objective-C declarations
//----------------------------------------------------------------------------

void DeclPrinter::VisitObjCMethodDecl(ObjCMethodDecl *OMD) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCMethodDecl>";
  }
 
  if (OMD->isInstanceMethod())
    Out << "- ";
  else
    Out << "+ ";
  if (!OMD->getReturnType().isNull())
    Out << '(' << OMD->getASTContext()
                      .getUnqualifiedObjCPointerType(OMD->getReturnType())
                      .getAsString(Policy) << ")";

  std::string name = OMD->getSelector().getAsString();
  std::string::size_type pos, lastPos = 0;
  for (const auto *PI : OMD->params()) {
    // FIXME: selector is missing here!
    pos = name.find_first_of(':', lastPos);
    Out << " " << name.substr(lastPos, pos - lastPos);
    Out << ":(" << PI->getASTContext().getUnqualifiedObjCPointerType(PI->getType()).
                      getAsString(Policy) << ')' << *PI;
    lastPos = pos + 1;
  }

  if (OMD->param_begin() == OMD->param_end())
    Out << " " << name;

  if (OMD->isVariadic())
      Out << ", ...";

  if (OMD->getBody() && !Policy.TerseOutput) {
    Out << ' ';
    //OMD->getBody()->printPretty(Out, nullptr, Policy);
    PrintStmt(OMD->getBody(), Out, nullptr, Policy, 0);
    Out << '\n';
  }
  else if (Policy.PolishForDeclaration)
    Out << ';';
    
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCMethodDecl>";
  }
}

void DeclPrinter::VisitObjCImplementationDecl(ObjCImplementationDecl *OID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCImplementationDecl>";
  }
  
  std::string I = OID->getNameAsString();
  ObjCInterfaceDecl *SID = OID->getSuperClass();

  if (SID)
    Out << "@implementation " << I << " : " << *SID;
  else
    Out << "@implementation " << I;
  
  if (OID->ivar_size() > 0) {
    Out << "{\n";
    Indentation += Policy.Indentation;
    for (const auto *I : OID->ivars()) {
      Indent() << I->getASTContext().getUnqualifiedObjCPointerType(I->getType()).
                    getAsString(Policy) << ' ' << *I << ";\n";
    }
    Indentation -= Policy.Indentation;
    Out << "}\n";
  }
  VisitDeclContext(OID, false);
  Out << "@end";
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCImplementationDecl>";
  }
}

void DeclPrinter::VisitObjCInterfaceDecl(ObjCInterfaceDecl *OID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCInterfaceDecl>";
  }
  
  std::string I = OID->getNameAsString();
  ObjCInterfaceDecl *SID = OID->getSuperClass();

  if (!OID->isThisDeclarationADefinition()) {
    Out << "@class " << I << ";";
    return;
  }
  bool eolnOut = false;
  if (SID)
    Out << "@interface " << I << " : " << *SID;
  else
    Out << "@interface " << I;

  // Protocols?
  const ObjCList<ObjCProtocolDecl> &Protocols = OID->getReferencedProtocols();
  if (!Protocols.empty()) {
    for (ObjCList<ObjCProtocolDecl>::iterator I = Protocols.begin(),
         E = Protocols.end(); I != E; ++I)
      Out << (I == Protocols.begin() ? '<' : ',') << **I;
    Out << "> ";
  }

  if (OID->ivar_size() > 0) {
    Out << "{\n";
    eolnOut = true;
    Indentation += Policy.Indentation;
    for (const auto *I : OID->ivars()) {
      Indent() << I->getASTContext()
                      .getUnqualifiedObjCPointerType(I->getType())
                      .getAsString(Policy) << ' ' << *I << ";\n";
    }
    Indentation -= Policy.Indentation;
    Out << "}\n";
  }
  else if (SID) {
    Out << "\n";
    eolnOut = true;
  }

  VisitDeclContext(OID, false);
  if (!eolnOut)
    Out << ' ';
  Out << "@end";
  // FIXME: implement the rest...
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCInterfaceDecl>";
  }
}

void DeclPrinter::VisitObjCProtocolDecl(ObjCProtocolDecl *PID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCProtocolDecl>";
  }
  
  if (!PID->isThisDeclarationADefinition()) {
    Out << "@protocol " << *PID << ";\n";
    return;
  }
  // Protocols?
  const ObjCList<ObjCProtocolDecl> &Protocols = PID->getReferencedProtocols();
  if (!Protocols.empty()) {
    Out << "@protocol " << *PID;
    for (ObjCList<ObjCProtocolDecl>::iterator I = Protocols.begin(),
         E = Protocols.end(); I != E; ++I)
      Out << (I == Protocols.begin() ? '<' : ',') << **I;
    Out << ">\n";
  } else
    Out << "@protocol " << *PID << '\n';
  VisitDeclContext(PID, false);
  Out << "@end";
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCProtocolDecl>";
  }
}

void DeclPrinter::VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *PID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCCategoryImplDecl>";
  }
  
  Out << "@implementation " << *PID->getClassInterface() << '(' << *PID <<")\n";

  VisitDeclContext(PID, false);
  Out << "@end";
  // FIXME: implement the rest...
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCCategoryImplDecl>";
  }
}

void DeclPrinter::VisitObjCCategoryDecl(ObjCCategoryDecl *PID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCCategoryDecl>";
  }
  
  Out << "@interface " << *PID->getClassInterface() << '(' << *PID << ")\n";
  if (PID->ivar_size() > 0) {
    Out << "{\n";
    Indentation += Policy.Indentation;
    for (const auto *I : PID->ivars())
      Indent() << I->getASTContext().getUnqualifiedObjCPointerType(I->getType()).
                    getAsString(Policy) << ' ' << *I << ";\n";
    Indentation -= Policy.Indentation;
    Out << "}\n";
  }
  
  VisitDeclContext(PID, false);
  Out << "@end";

  // FIXME: implement the rest...
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCCategoryDecl>";
  }
}

void DeclPrinter::VisitObjCCompatibleAliasDecl(ObjCCompatibleAliasDecl *AID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCCompatibleAliasDecl>";
  }
  
  Out << "@compatibility_alias " << *AID
      << ' ' << *AID->getClassInterface() << ";\n";
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCCompatibleAliasDecl>";
  }
}

/// PrintObjCPropertyDecl - print a property declaration.
///
void DeclPrinter::VisitObjCPropertyDecl(ObjCPropertyDecl *PDecl) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCPropertyDecl>";
  }
  
  if (PDecl->getPropertyImplementation() == ObjCPropertyDecl::Required)
    Out << "@required\n";
  else if (PDecl->getPropertyImplementation() == ObjCPropertyDecl::Optional)
    Out << "@optional\n";

  Out << "@property";
  if (PDecl->getPropertyAttributes() != ObjCPropertyDecl::OBJC_PR_noattr) {
    bool first = true;
    Out << " (";
    if (PDecl->getPropertyAttributes() &
        ObjCPropertyDecl::OBJC_PR_readonly) {
      Out << (first ? ' ' : ',') << "readonly";
      first = false;
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_getter) {
      Out << (first ? ' ' : ',') << "getter = ";
      PDecl->getGetterName().print(Out);
      first = false;
    }
    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_setter) {
      Out << (first ? ' ' : ',') << "setter = ";
      PDecl->getSetterName().print(Out);
      first = false;
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_assign) {
      Out << (first ? ' ' : ',') << "assign";
      first = false;
    }

    if (PDecl->getPropertyAttributes() &
        ObjCPropertyDecl::OBJC_PR_readwrite) {
      Out << (first ? ' ' : ',') << "readwrite";
      first = false;
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_retain) {
      Out << (first ? ' ' : ',') << "retain";
      first = false;
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_strong) {
      Out << (first ? ' ' : ',') << "strong";
      first = false;
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_copy) {
      Out << (first ? ' ' : ',') << "copy";
      first = false;
    }

    if (PDecl->getPropertyAttributes() &
        ObjCPropertyDecl::OBJC_PR_nonatomic) {
      Out << (first ? ' ' : ',') << "nonatomic";
      first = false;
    }
    if (PDecl->getPropertyAttributes() &
        ObjCPropertyDecl::OBJC_PR_atomic) {
      Out << (first ? ' ' : ',') << "atomic";
      first = false;
    }
    
    (void) first; // Silence dead store warning due to idiomatic code.
    Out << " )";
  }
  Out << ' ' << PDecl->getASTContext().getUnqualifiedObjCPointerType(PDecl->getType()).
                  getAsString(Policy) << ' ' << *PDecl;
  if (Policy.PolishForDeclaration)
    Out << ';';
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCPropertyDecl>";
  }
}

void DeclPrinter::VisitObjCPropertyImplDecl(ObjCPropertyImplDecl *PID) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<ObjCPropertyImplDecl>";
  }
  
  if (PID->getPropertyImplementation() == ObjCPropertyImplDecl::Synthesize)
    Out << "@synthesize ";
  else
    Out << "@dynamic ";
  Out << *PID->getPropertyDecl();
  if (PID->getPropertyIvarDecl())
    Out << '=' << *PID->getPropertyIvarDecl();
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</ObjCPropertyImplDecl>";
  }
}

void DeclPrinter::VisitUsingDecl(UsingDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<UsingDecl>";
  }
  
  if (!D->isAccessDeclaration())
    Out << "using ";
  if (D->hasTypename())
    Out << "typename ";
  D->getQualifier()->print(Out, Policy);
  Out << *D;
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</UsingDecl>";
  }
}

void DeclPrinter::VisitUnresolvedUsingTypenameDecl(UnresolvedUsingTypenameDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<UnresolvedUsingTypenameDecl>";
  }
  
  Out << "using typename ";
  D->getQualifier()->print(Out, Policy);
  Out << D->getDeclName();
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</UnresolvedUsingTypenameDecl>";
  }
}

void DeclPrinter::VisitUnresolvedUsingValueDecl(UnresolvedUsingValueDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<UnresolvedUsingValueDecl>";
  }
  
  if (!D->isAccessDeclaration())
    Out << "using ";
  D->getQualifier()->print(Out, Policy);
  Out << D->getName();
  
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</UnresolvedUsingValueDecl>";
  }
}

void DeclPrinter::VisitUsingShadowDecl(UsingShadowDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<UsingShadowDecl>";
  }
  // ignore
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</UsingShadowDecl>";
  }
}

void DeclPrinter::VisitOMPThreadPrivateDecl(OMPThreadPrivateDecl *D) {
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"<OMPThreadPrivateDecl>";
  }
  Out << "#pragma omp threadprivate";
  if (!D->varlist_empty()) {
    for (OMPThreadPrivateDecl::varlist_iterator I = D->varlist_begin(),
                                                E = D->varlist_end();
                                                I != E; ++I) {
      Out << (I == D->varlist_begin() ? '(' : ',');
      NamedDecl *ND = cast<NamedDecl>(cast<DeclRefExpr>(*I)->getDecl());
      ND->printQualifiedName(Out);
    }
    Out << ")";
  }
  if(Policy.SuppressDeclTag == false)
  {
  	Out<<"</OMPThreadPrivateDecl>";
  }
}


}//namespace another_printer

