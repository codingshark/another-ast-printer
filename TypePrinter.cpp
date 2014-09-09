//===--- TypePrinter.cpp - Pretty-Print Clang Types -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to print types from Clang's type system.
//
//===----------------------------------------------------------------------===//

#include "TypePrinter.h"
#include "StmtPrinter.h"

/*

std::string Qualifiers::getAsString() const {
  LangOptions LO;
  return getAsString(PrintingPolicy(LO));
}

// Appends qualifiers to the given string, separated by spaces.  Will
// prefix a space if the string is non-empty.  Will not append a final
// space.
std::string Qualifiers::getAsString(const PrintingPolicy &Policy) const {
  SmallString<64> Buf;
  llvm::raw_svector_ostream StrOS(Buf);
  print(StrOS, Policy);
  return StrOS.str();
}

bool Qualifiers::isEmptyWhenPrinted(const PrintingPolicy &Policy) const {
  if (getCVRQualifiers())
    return false;

  if (getAddressSpace())
    return false;

  if (getObjCGCAttr())
    return false;

  if (Qualifiers::ObjCLifetime lifetime = getObjCLifetime())
    if (!(lifetime == Qualifiers::OCL_Strong && Policy.SuppressStrongLifetime))
      return false;

  return true;
}
*/

namespace another_printer{

static void printIntegral(const TemplateArgument &TemplArg,
                           raw_ostream &Out, const another_printer::PrintingPolicy &Policy) {
   const ::clang::Type *T = TemplArg.getIntegralType().getTypePtr();
   const llvm::APSInt &Val = TemplArg.getAsIntegral();
   
  if(Policy.SuppressTypeTag == false)
  {
  	Out<<"<printIntegral>";
  }
   if (T->isBooleanType()) {
     Out << (Val.getBoolValue() ? "true" : "false");
   } else if (T->isCharType()) {
     const char Ch = Val.getZExtValue();
     Out << ((Ch == '\'') ? "'\\" : "'");
     Out.write_escaped(StringRef(&Ch, 1), /*UseHexEscapes=*/ true);
     Out << "'";
   } else {
     Out << Val;
   }
   if(Policy.SuppressTypeTag == false)
  {
  	Out<<"</printIntegral>";
  }
 }

static void AppendTypeQualList(raw_ostream &OS, unsigned TypeQuals) {
  bool appendSpace = false;
  if (TypeQuals & Qualifiers::Const) {
    OS << "const";
    appendSpace = true;
  }
  if (TypeQuals & Qualifiers::Volatile) {
    if (appendSpace) OS << ' ';
    OS << "volatile";
    appendSpace = true;
  }
  if (TypeQuals & Qualifiers::Restrict) {
    if (appendSpace) OS << ' ';
    OS << "restrict";
  }
}

void 
TypePrinter::printExceptionSpecification(const FunctionProtoType * T, raw_ostream &OS, 
                                               const PrintingPolicy &Policy) const {
  if(Policy.SuppressTypeTag == false)
  {
     OS<<"<printExceptionSpecification>";
  }
  if (T->hasDynamicExceptionSpec()) {
    OS << " throw(";
    if (T->getExceptionSpecType() == EST_MSAny)
      OS << "...";
    else
      for (unsigned I = 0, N = T->getNumExceptions(); I != N; ++I) {
        if (I)
          OS << ", ";
        
        OS << T->getExceptionType(I).stream(Policy);
      }
    OS << ')';
  } else if (isNoexceptExceptionSpec(T->getExceptionSpecType())) {
    OS << " noexcept";
    if (T->getExceptionSpecType() == EST_ComputedNoexcept) {
      OS << '(';
      if (T->getNoexceptExpr())
      	StmtPrinter::PrintStmt(T->getNoexceptExpr(), OS, nullptr, Policy);
        //getNoexceptExpr()->printPretty(OS, nullptr, Policy);
      OS << ')';
    }
  }
  if(Policy.SuppressTypeTag == false)
  {
     OS<<"</printExceptionSpecification>";
  }
}

void TypePrinter::print(const Type *ty, Qualifiers qs,
                      raw_ostream &OS, const PrintingPolicy &policy,
                      const Twine &PlaceHolder) {
   SmallString<128> PHBuf;
   StringRef PH = PlaceHolder.toStringRef(PHBuf);
 
   TypePrinter(policy).print(ty, qs, OS, PH);
 }


// Appends qualifiers to the given string, separated by spaces.  Will
// prefix a space if the string is non-empty.  Will not append a final
// space.
void TypePrinter::printQualifiers(const Qualifiers & T, raw_ostream &OS, const another_printer::PrintingPolicy& Policy,
                       bool appendSpaceIfNonEmpty) const {
  if(Policy.SuppressTypeTag == false)
  {
     OS<<"<Qualifiers>";
  }
  bool addSpace = false;

  unsigned quals = T.getCVRQualifiers();
  if (quals) {
    AppendTypeQualList(OS, quals);
    addSpace = true;
  }
  if (unsigned addrspace = T.getAddressSpace()) {
    if (addSpace)
      OS << ' ';
    addSpace = true;
    switch (addrspace) {
      case LangAS::opencl_global:
        OS << "__global";
        break;
      case LangAS::opencl_local:
        OS << "__local";
        break;
      case LangAS::opencl_constant:
        OS << "__constant";
        break;
      default:
        OS << "__attribute__((address_space(";
        OS << addrspace;
        OS << ")))";
    }
  }
  if (Qualifiers::GC gc = T.getObjCGCAttr()) {
    if (addSpace)
      OS << ' ';
    addSpace = true;
    if (gc == Qualifiers::Weak)
      OS << "__weak";
    else
      OS << "__strong";
  }
  if (Qualifiers::ObjCLifetime lifetime = T.getObjCLifetime()) {
    if (!(lifetime == Qualifiers::OCL_Strong && Policy.SuppressStrongLifetime)){
      if (addSpace)
        OS << ' ';
      addSpace = true;
    }

    switch (lifetime) {
    case Qualifiers::OCL_None: llvm_unreachable("none but true");
    case Qualifiers::OCL_ExplicitNone: OS << "__unsafe_unretained"; break;
    case Qualifiers::OCL_Strong: 
      if (!Policy.SuppressStrongLifetime)
        OS << "__strong"; 
      break;
        
    case Qualifiers::OCL_Weak: OS << "__weak"; break;
    case Qualifiers::OCL_Autoreleasing: OS << "__autoreleasing"; break;
    }
  }

  if (appendSpaceIfNonEmpty && addSpace)
    OS << ' ';
 
 if(Policy.SuppressTypeTag == false)
  {
     OS<<"</Qualifiers>";
  }
}

//modified from NestedNameSpecifier::print(raw_ostream &OS, const PrintingPolicy &Policy) const {
/// \brief Print this nested name specifier to the given output
/// stream.
void TypePrinter::printNestedNameSpecifier(NestedNameSpecifier * T, raw_ostream &OS,  const another_printer::PrintingPolicy & Policy) const
{
  if (T->getPrefix())
     printNestedNameSpecifier(T->getPrefix(), OS, Policy);
 
   switch (T->getKind()) {
   case clang::NestedNameSpecifier::Identifier:
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::Identifier>";
     }
     OS << T->getAsIdentifier()->getName();
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"</NestedNameSpecifier::Identifier>";
     }
     break;
 
   case clang::NestedNameSpecifier::Namespace:
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::Namespace>";
     }
     if (T->getAsNamespace()->isAnonymousNamespace())
     {
       if(Policy.SuppressTypeTag == false)
       {
          OS<<"</NestedNameSpecifier::Namespace>";
       }
       return;
     }
     OS << T->getAsNamespace()->getName();
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"</NestedNameSpecifier::Namespace>";
     }
     break;

   case clang::NestedNameSpecifier::NamespaceAlias:
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::NamespaceAlias>";
     }
     OS << T->getAsNamespaceAlias()->getName();
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"</NestedNameSpecifier::NamespaceAlias>";
     }
     break;
     
   case clang::NestedNameSpecifier::Global:
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::Global>";
        OS<<"</NestedNameSpecifier::Global>";
     }
     break;
 
   case clang::NestedNameSpecifier::TypeSpecWithTemplate:
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::TypeSpecWithTemplate>";
     }
     OS << "template ";
     // Fall through to print the type.
 
   case clang::NestedNameSpecifier::TypeSpec: {
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"<NestedNameSpecifier::TypeSpec>";
     }
     const Type * tt = T->getAsType();
 
     another_printer::PrintingPolicy InnerPolicy(Policy);
     InnerPolicy.SuppressScope = true;
 
     // Nested-name-specifiers are intended to contain minimally-qualified
     // types. An actual ElaboratedType will not occur, since we'll store
     // just the type that is referred to in the nested-name-specifier (e.g.,
     // a TypedefType, TagType, etc.). However, when we are dealing with
     // dependent template-id types (e.g., Outer<T>::template Inner<U>),
     // the type requires its own nested-name-specifier for uniqueness, so we
     // suppress that nested-name-specifier during printing.
     assert(!isa<ElaboratedType>(tt) &&
            "Elaborated type in nested-name-specifier");
     if (const TemplateSpecializationType *SpecType
           = dyn_cast<TemplateSpecializationType>(tt)) {
       // Print the template name without its corresponding
       // nested-name-specifier.
       printTemplateName(SpecType->getTemplateName(), OS, InnerPolicy, true);
 
       // Print the template argument list.
       TypePrinter::PrintTemplateArgumentList(
           OS, SpecType->getArgs(), SpecType->getNumArgs(), InnerPolicy);
     } else {
       // Print the type normally
         QualType qtype(tt, 0);
         printQualType(qtype, OS, InnerPolicy);
     }
     if(Policy.SuppressTypeTag == false)
     {
        OS<<"</NestedNameSpecifier::TypeSpec>";
     }
     if(T->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
     {
       if(Policy.SuppressTypeTag == false)
       {
          OS<<"</NestedNameSpecifier::TypeSpecWithTemplate>";
       }
     }
     break;
   }
   }
 
   OS << "::";
}

// Sadly, repeat all that with TemplateArgLoc.
void TypePrinter::PrintTemplateArgumentList(raw_ostream &OS,
                          const TemplateArgumentLoc *Args, unsigned NumArgs,
                          const another_printer::PrintingPolicy &Policy) const{
  OS << '<';

  bool needSpace = false;
  for (unsigned Arg = 0; Arg < NumArgs; ++Arg) {
    if (Arg > 0)
      OS << ", ";
    
    // Print the argument into a string.
    SmallString<128> Buf;
    llvm::raw_svector_ostream ArgOS(Buf);
    if (Args[Arg].getArgument().getKind() == TemplateArgument::Pack) {
      TypePrinter::PrintTemplateArgumentList(ArgOS,
                                Args[Arg].getArgument().pack_begin(), 
                                Args[Arg].getArgument().pack_size(), 
                                Policy, true);
    } else {
      printTemplateArgument(Args[Arg].getArgument(), Policy, ArgOS);
    }
    StringRef ArgString = ArgOS.str();
    
    // If this is the first argument and its string representation
    // begins with the global scope specifier ('::foo'), add a space
    // to avoid printing the diagraph '<:'.
    if (!Arg && !ArgString.empty() && ArgString[0] == ':')
      OS << ' ';

    OS << ArgString;

    needSpace = (!ArgString.empty() && ArgString.back() == '>');
  }
  // If the last character of our string is '>', add another space to
  // keep the two '>''s separate tokens. We don't *have* to do this in
  // C++0x, but it's still good hygiene.
  if (needSpace)
    OS << ' ';

  OS << '>';
}

void
TypePrinter::PrintTemplateArgumentList(raw_ostream &OS,
                                                const TemplateArgument *Args, unsigned NumArgs,
                                                  const another_printer::PrintingPolicy &Policy, bool SkipBrackets) const
{
  if (!SkipBrackets)
    OS << '<';
  
  bool needSpace = false;
  for (unsigned Arg = 0; Arg < NumArgs; ++Arg) {
    // Print the argument into a string.
    SmallString<128> Buf;
    llvm::raw_svector_ostream ArgOS(Buf);
    if (Args[Arg].getKind() == TemplateArgument::Pack) {
      if (Args[Arg].pack_size() && Arg > 0)
        OS << ", ";
      TypePrinter::PrintTemplateArgumentList(ArgOS,
                                Args[Arg].pack_begin(), 
                                Args[Arg].pack_size(), 
                                Policy, true);
    } else {
      if (Arg > 0)
        OS << ", ";
       printTemplateArgument(Args[Arg], Policy, ArgOS);
    }
    StringRef ArgString = ArgOS.str();

    // If this is the first argument and its string representation
    // begins with the global scope specifier ('::foo'), add a space
    // to avoid printing the diagraph '<:'.
    if (!Arg && !ArgString.empty() && ArgString[0] == ':')
      OS << ' ';

    OS << ArgString;

    needSpace = (!ArgString.empty() && ArgString.back() == '>');
  }

  // If the last character of our string is '>', add another space to
  // keep the two '>''s separate tokens. We don't *have* to do this in
  // C++0x, but it's still good hygiene.
  if (needSpace)
    OS << ' ';

  if (!SkipBrackets)
    OS << '>';
}

void TypePrinter::PrintTemplateArgumentList(raw_ostream &OS,
                            const TemplateArgumentListInfo &Args,
                            const PrintingPolicy &Policy) const {
  return TypePrinter::PrintTemplateArgumentList(OS,
                                   Args.getArgumentArray(),
                                   Args.size(),
                                   Policy);
}

void TypePrinter::printTemplateName(const TemplateName & T, raw_ostream &OS, const another_printer::PrintingPolicy &Policy,
                     bool SuppressNNS) const {
   if(Policy.SuppressTypeTag == false)
   {
  	OS<<"<TemplateName>";
   }

   if (QualifiedTemplateName *QTN = T.getAsQualifiedTemplateName()) {
     if (!SuppressNNS)
       printNestedNameSpecifier(QTN->getQualifier(), OS, Policy);
     if (QTN->hasTemplateKeyword())
       OS << "template ";
       OS << *QTN->getDecl();
   } else if (DependentTemplateName *DTN = T.getAsDependentTemplateName()) {
     if (!SuppressNNS && DTN->getQualifier())
       printNestedNameSpecifier(DTN->getQualifier(), OS, Policy);
       //DTN->getQualifier()->print(OS, Policy);
     OS << "template ";
     
     if (DTN->isIdentifier())
       OS << DTN->getIdentifier()->getName();
     else
       OS << "operator " << getOperatorSpelling(DTN->getOperator());
   } else if (SubstTemplateTemplateParmStorage *subst
                = T.getAsSubstTemplateTemplateParm()) {
     subst->getReplacement().print(OS, Policy, SuppressNNS);
   } else if (SubstTemplateTemplateParmPackStorage *SubstPack
                                         = T.getAsSubstTemplateTemplateParmPack())
     OS << *SubstPack->getParameterPack();
   else if(OverloadedTemplateStorage *OTS = T.getAsOverloadedTemplate()){
     //OverloadedTemplateStorage *OTS = T.getAsOverloadedTemplate();
     (*OTS->begin())->printName(OS);
   }else
   {
   	TemplateDecl *Template = static_cast<TemplateDecl *>(T.getAsVoidPointer());
        OS << *Template;
   }
   if(Policy.SuppressTypeTag == false)
   {
  	OS<<"</TemplateName>";
   }
 }

void TypePrinter::printTemplateArgument(const TemplateArgument & T, const another_printer::PrintingPolicy &Policy, 
                              raw_ostream &Out) const {
   if(Policy.SuppressTypeTag == false)
   {
  	Out<<"<TemplateArgument>";
   }  
   switch (T.getKind()) {
   case clang::TemplateArgument::Null:
     Out << "(no value)";
     break;
     
   case clang::TemplateArgument::Type: {
     another_printer::PrintingPolicy SubPolicy(Policy);
     SubPolicy.SuppressStrongLifetime = true;
     printQualType(T.getAsType(), Out, SubPolicy);
     break;
   }
     
   case clang::TemplateArgument::Declaration: {
     NamedDecl *ND = cast<NamedDecl>(T.getAsDecl());
     Out << '&';
     if (ND->getDeclName()) {
       // FIXME: distinguish between pointer and reference args?
       ND->printQualifiedName(Out);
     } else {
       Out << "(anonymous)";
     }
     break;
   }
 
   case clang::TemplateArgument::NullPtr:
     Out << "nullptr";
     break;
 
   case clang::TemplateArgument::Template:
     printTemplateName(T.getAsTemplate(), Out, Policy);
//     getAsTemplate().print(Out, Policy);
     break;
 
   case clang::TemplateArgument::TemplateExpansion:
//     T.getAsTemplateOrTemplatePattern().print(Out, Policy);
     printTemplateName(T.getAsTemplateOrTemplatePattern(), Out, Policy);
     Out << "...";
     break;
       
   case clang::TemplateArgument::Integral: {
     printIntegral(T, Out, Policy);
     break;
   }
     
   case clang::TemplateArgument::Expression:
     //getAsExpr()->printPretty(Out, nullptr, Policy);
     another_printer::StmtPrinter::PrintStmt(T.getAsExpr(), Out, nullptr, Policy);
     break;
     
   case clang::TemplateArgument::Pack:
     Out << "<";
     bool First = true;
     for (const auto &P : T.pack_elements()) {
       if (First)
         First = false;
       else
         Out << ", ";
       
//       P.print(Policy, Out);
       printTemplateArgument(P, Policy, Out);
     }
     Out << ">";
     break;        
   }
   if(Policy.SuppressTypeTag == false)
   {
  	Out<<"</TemplateArgument>";
   }  
 }

void TypePrinter::spaceBeforePlaceHolder(raw_ostream &OS) {
  if (!HasEmptyPlaceHolder)
    OS << ' ';
}

void TypePrinter::printQualType(const QualType & t, raw_ostream &OS, StringRef PlaceHolder) {
  SplitQualType split = t.split();
  print(split.Ty, split.Quals, OS, PlaceHolder);
}

void TypePrinter::print(const Type *T, Qualifiers Quals, raw_ostream &OS,
                        StringRef PlaceHolder) {
  if (!T) {
    OS << "NULL TYPE";
    return;
  }

  SaveAndRestore<bool> PHVal(HasEmptyPlaceHolder, PlaceHolder.empty());

  printBefore(T, Quals, OS);
  OS << PlaceHolder;
  printAfter(T, Quals, OS);
}

bool TypePrinter::canPrefixQualifiers(const Type *T,
                                      bool &NeedARCStrongQualifier) {
  // CanPrefixQualifiers - We prefer to print type qualifiers before the type,
  // so that we get "const int" instead of "int const", but we can't do this if
  // the type is complex.  For example if the type is "int*", we *must* print
  // "int * const", printing "const int *" is different.  Only do this when the
  // type expands to a simple string.
  bool CanPrefixQualifiers = false;
  NeedARCStrongQualifier = false;
  Type::TypeClass TC = T->getTypeClass();
  if (const AutoType *AT = dyn_cast<AutoType>(T))
    TC = AT->desugar()->getTypeClass();
  if (const SubstTemplateTypeParmType *Subst
                                      = dyn_cast<SubstTemplateTypeParmType>(T))
    TC = Subst->getReplacementType()->getTypeClass();
  
  switch (TC) {
    case Type::Auto:
    case Type::Builtin:
    case Type::Complex:
    case Type::UnresolvedUsing:
    case Type::Typedef:
    case Type::TypeOfExpr:
    case Type::TypeOf:
    case Type::Decltype:
    case Type::UnaryTransform:
    case Type::Record:
    case Type::Enum:
    case Type::Elaborated:
    case Type::TemplateTypeParm:
    case Type::SubstTemplateTypeParmPack:
    case Type::TemplateSpecialization:
    case Type::InjectedClassName:
    case Type::DependentName:
    case Type::DependentTemplateSpecialization:
    case Type::ObjCObject:
    case Type::ObjCInterface:
    case Type::Atomic:
      CanPrefixQualifiers = true;
      break;
      
    case Type::ObjCObjectPointer:
      CanPrefixQualifiers = T->isObjCIdType() || T->isObjCClassType() ||
        T->isObjCQualifiedIdType() || T->isObjCQualifiedClassType();
      break;
      
    case Type::ConstantArray:
    case Type::IncompleteArray:
    case Type::VariableArray:
    case Type::DependentSizedArray:
      NeedARCStrongQualifier = true;
      // Fall through
      
    case Type::Adjusted:
    case Type::Decayed:
    case Type::Pointer:
    case Type::BlockPointer:
    case Type::LValueReference:
    case Type::RValueReference:
    case Type::MemberPointer:
    case Type::DependentSizedExtVector:
    case Type::Vector:
    case Type::ExtVector:
    case Type::FunctionProto:
    case Type::FunctionNoProto:
    case Type::Paren:
    case Type::Attributed:
    case Type::PackExpansion:
    case Type::SubstTemplateTypeParm:
      CanPrefixQualifiers = false;
      break;
  }

  return CanPrefixQualifiers;
}

void TypePrinter::printBefore(QualType T, raw_ostream &OS) {
  SplitQualType Split = T.split();

  // If we have cv1 T, where T is substituted for cv2 U, only print cv1 - cv2
  // at this level.
  Qualifiers Quals = Split.Quals;
  if (const SubstTemplateTypeParmType *Subst =
        dyn_cast<SubstTemplateTypeParmType>(Split.Ty))
    Quals -= QualType(Subst, 0).getQualifiers();

  printBefore(Split.Ty, Quals, OS);
}

/// \brief Prints the part of the type string before an identifier, e.g. for
/// "int foo[10]" it prints "int ".
void TypePrinter::printBefore(const Type *T,Qualifiers Quals, raw_ostream &OS) {
  if (Policy.SuppressSpecifiers && T->isSpecifierType())
    return;

  SaveAndRestore<bool> PrevPHIsEmpty(HasEmptyPlaceHolder);

  // Print qualifiers as appropriate.

  bool CanPrefixQualifiers = false;
  bool NeedARCStrongQualifier = false;
  CanPrefixQualifiers = canPrefixQualifiers(T, NeedARCStrongQualifier);

  if (CanPrefixQualifiers && !Quals.empty()) {
    if (NeedARCStrongQualifier) {
      IncludeStrongLifetimeRAII Strong(Policy);
      Quals.print(OS, Policy, /*appendSpaceIfNonEmpty=*/true);
    } else {
      Quals.print(OS, Policy, /*appendSpaceIfNonEmpty=*/true);
    }
  }

  bool hasAfterQuals = false;
  if (!CanPrefixQualifiers && !Quals.empty()) {
    hasAfterQuals = !Quals.isEmptyWhenPrinted(Policy);
    if (hasAfterQuals)
      HasEmptyPlaceHolder = false;
  }

  switch (T->getTypeClass()) {
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT) case Type::CLASS: \
    print##CLASS##Before(cast<CLASS##Type>(T), OS); \
    break;
#include "clang/AST/TypeNodes.def"
  }

  if (hasAfterQuals) {
    if (NeedARCStrongQualifier) {
      IncludeStrongLifetimeRAII Strong(Policy);
      Quals.print(OS, Policy, /*appendSpaceIfNonEmpty=*/!PrevPHIsEmpty.get());
    } else {
      Quals.print(OS, Policy, /*appendSpaceIfNonEmpty=*/!PrevPHIsEmpty.get());
    }
  }
}

void TypePrinter::printAfter(QualType t, raw_ostream &OS) {
  SplitQualType split = t.split();
  printAfter(split.Ty, split.Quals, OS);
}

/// \brief Prints the part of the type string after an identifier, e.g. for
/// "int foo[10]" it prints "[10]".
void TypePrinter::printAfter(const Type *T, Qualifiers Quals, raw_ostream &OS) {
  switch (T->getTypeClass()) {
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT) case Type::CLASS: \
    print##CLASS##After(cast<CLASS##Type>(T), OS); \
    break;
#include "clang/AST/TypeNodes.def"
  }
}

void TypePrinter::printBuiltinBefore(const BuiltinType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<BuiltinType>";
  }
  OS << T->getName(Policy);
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printBuiltinAfter(const BuiltinType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</BuiltinType>";
  }
}

void TypePrinter::printComplexBefore(const ComplexType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ComplexType>";
  }
  OS << "_Complex ";
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printComplexAfter(const ComplexType *T, raw_ostream &OS) {
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ComplexType>";
  }
}

void TypePrinter::printPointerBefore(const PointerType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<PointerType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getPointeeType(), OS);
  // Handle things like 'int (*A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeType()))
    OS << '(';
  OS << '*';
}
void TypePrinter::printPointerAfter(const PointerType *T, raw_ostream &OS) {
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  // Handle things like 'int (*A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeType()))
    OS << ')';
  printAfter(T->getPointeeType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</PointerType>";
  }
}

void TypePrinter::printBlockPointerBefore(const BlockPointerType *T,
                                          raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<BlockPointerType>";
  }
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getPointeeType(), OS);
  OS << '^';
}
void TypePrinter::printBlockPointerAfter(const BlockPointerType *T,
                                          raw_ostream &OS) {
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printAfter(T->getPointeeType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</BlockPointerType>";
  }
}

void TypePrinter::printLValueReferenceBefore(const LValueReferenceType *T,
                                             raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<LValueReferenceType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getPointeeTypeAsWritten(), OS);
  // Handle things like 'int (&A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeTypeAsWritten()))
    OS << '(';
  OS << '&';
}
void TypePrinter::printLValueReferenceAfter(const LValueReferenceType *T,
                                            raw_ostream &OS) {
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  // Handle things like 'int (&A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeTypeAsWritten()))
    OS << ')';
  printAfter(T->getPointeeTypeAsWritten(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</LValueReferenceType>";
  }
}

void TypePrinter::printRValueReferenceBefore(const RValueReferenceType *T,
                                             raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<RValueReferenceType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getPointeeTypeAsWritten(), OS);
  // Handle things like 'int (&&A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeTypeAsWritten()))
    OS << '(';
  OS << "&&";
}
void TypePrinter::printRValueReferenceAfter(const RValueReferenceType *T,
                                            raw_ostream &OS) {
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  // Handle things like 'int (&&A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeTypeAsWritten()))
    OS << ')';
  printAfter(T->getPointeeTypeAsWritten(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</RValueReferenceType>";
  }
}

void TypePrinter::printMemberPointerBefore(const MemberPointerType *T, 
                                           raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<MemberPointerType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getPointeeType(), OS);
  // Handle things like 'int (Cls::*A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeType()))
    OS << '(';

  PrintingPolicy InnerPolicy(Policy);
  InnerPolicy.SuppressTag = false;
  TypePrinter(InnerPolicy).printQualType(QualType(T->getClass(), 0), OS, StringRef());

  OS << "::*";
}
void TypePrinter::printMemberPointerAfter(const MemberPointerType *T, 
                                          raw_ostream &OS) { 
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  // Handle things like 'int (Cls::*A)[4];' correctly.
  // FIXME: this should include vectors, but vectors use attributes I guess.
  if (isa<ArrayType>(T->getPointeeType()))
    OS << ')';
  printAfter(T->getPointeeType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</MemberPointerType>";
  }
}

void TypePrinter::printConstantArrayBefore(const ConstantArrayType *T, 
                                           raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ConstantArrayType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printConstantArrayAfter(const ConstantArrayType *T, 
                                          raw_ostream &OS) {
  OS << '[';
  if (T->getIndexTypeQualifiers().hasQualifiers()) {
    AppendTypeQualList(OS, T->getIndexTypeCVRQualifiers());
    OS << ' ';
  }

  if (T->getSizeModifier() == ArrayType::Static)
    OS << "static ";

  OS << T->getSize().getZExtValue() << ']';
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ConstantArrayType>";
  }
}

void TypePrinter::printIncompleteArrayBefore(const IncompleteArrayType *T, 
                                             raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<IncompleteArrayType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printIncompleteArrayAfter(const IncompleteArrayType *T, 
                                            raw_ostream &OS) {
  OS << "[]";
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</IncompleteArrayType>";
  }
}

void TypePrinter::printVariableArrayBefore(const VariableArrayType *T, 
                                           raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<VariableArrayType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printVariableArrayAfter(const VariableArrayType *T, 
                                          raw_ostream &OS) {
  OS << '[';
  if (T->getIndexTypeQualifiers().hasQualifiers()) {
    AppendTypeQualList(OS, T->getIndexTypeCVRQualifiers());
    OS << ' ';
  }

  if (T->getSizeModifier() == VariableArrayType::Static)
    OS << "static ";
  else if (T->getSizeModifier() == VariableArrayType::Star)
    OS << '*';

  if (T->getSizeExpr())
    T->getSizeExpr()->printPretty(OS, nullptr, Policy);
  OS << ']';

  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</VariableArrayType>";
  }
}

void TypePrinter::printAdjustedBefore(const AdjustedType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<AdjustedType>";
  }
  // Print the adjusted representation, otherwise the adjustment will be
  // invisible.
  printBefore(T->getAdjustedType(), OS);
}
void TypePrinter::printAdjustedAfter(const AdjustedType *T, raw_ostream &OS) {
  printAfter(T->getAdjustedType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</AdjustedType>";
  }
}

void TypePrinter::printDecayedBefore(const DecayedType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DecayedType>";
  }
  // Print as though it's a pointer.
  printAdjustedBefore(T, OS);
}
void TypePrinter::printDecayedAfter(const DecayedType *T, raw_ostream &OS) {
  printAdjustedAfter(T, OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DecayedType>";
  }
}

void TypePrinter::printDependentSizedArrayBefore(
                                               const DependentSizedArrayType *T, 
                                               raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DependentSizedArrayType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printDependentSizedArrayAfter(
                                               const DependentSizedArrayType *T, 
                                               raw_ostream &OS) {
  OS << '[';
  if (T->getSizeExpr())
    T->getSizeExpr()->printPretty(OS, nullptr, Policy);
  OS << ']';
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DependentSizedArrayType>";
  }
}

void TypePrinter::printDependentSizedExtVectorBefore(
                                          const DependentSizedExtVectorType *T, 
                                          raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DependentSizedExtVectorType>";
  }
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printDependentSizedExtVectorAfter(
                                          const DependentSizedExtVectorType *T, 
                                          raw_ostream &OS) { 
  OS << " __attribute__((ext_vector_type(";
  if (T->getSizeExpr())
    T->getSizeExpr()->printPretty(OS, nullptr, Policy);
  OS << ")))";  
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DependentSizedExtVectorType>";
  }
}

void TypePrinter::printVectorBefore(const VectorType *T, raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<VectorType>";
  }
  switch (T->getVectorKind()) {
  case VectorType::AltiVecPixel:
    OS << "__vector __pixel ";
    break;
  case VectorType::AltiVecBool:
    OS << "__vector __bool ";
    printBefore(T->getElementType(), OS);
    break;
  case VectorType::AltiVecVector:
    OS << "__vector ";
    printBefore(T->getElementType(), OS);
    break;
  case VectorType::NeonVector:
    OS << "__attribute__((neon_vector_type("
       << T->getNumElements() << "))) ";
    printBefore(T->getElementType(), OS);
    break;
  case VectorType::NeonPolyVector:
    OS << "__attribute__((neon_polyvector_type(" <<
          T->getNumElements() << "))) ";
    printBefore(T->getElementType(), OS);
    break;
  case VectorType::GenericVector: {
    // FIXME: We prefer to print the size directly here, but have no way
    // to get the size of the type.
    OS << "__attribute__((__vector_size__("
       << T->getNumElements()
       << " * sizeof(";
    printQualType(T->getElementType(), OS, StringRef());
    OS << ")))) "; 
    printBefore(T->getElementType(), OS);
    break;
  }
  }
}
void TypePrinter::printVectorAfter(const VectorType *T, raw_ostream &OS) {
  printAfter(T->getElementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</VectorType>";
  }
} 

void TypePrinter::printExtVectorBefore(const ExtVectorType *T,
                                       raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ExtVectorType>";
  }
  printBefore(T->getElementType(), OS);
}
void TypePrinter::printExtVectorAfter(const ExtVectorType *T, raw_ostream &OS) { 
  printAfter(T->getElementType(), OS);
  OS << " __attribute__((ext_vector_type(";
  OS << T->getNumElements();
  OS << ")))";
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ExtVectorType>";
  }
}

void TypePrinter::printFunctionProtoBefore(const FunctionProtoType *T, 
                                           raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<FunctionProtoType>";
  }
  if (T->hasTrailingReturn()) {
    OS << "auto ";
    if (!HasEmptyPlaceHolder)
      OS << '(';
  } else {
    // If needed for precedence reasons, wrap the inner part in grouping parens.
    SaveAndRestore<bool> PrevPHIsEmpty(HasEmptyPlaceHolder, false);
    printBefore(T->getReturnType(), OS);
    if (!PrevPHIsEmpty.get())
      OS << '(';
  }
}

void TypePrinter::printFunctionProtoAfter(const FunctionProtoType *T, 
                                          raw_ostream &OS) { 
  // If needed for precedence reasons, wrap the inner part in grouping parens.
  if (!HasEmptyPlaceHolder)
    OS << ')';
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);

  OS << '(';
  {
    ParamPolicyRAII ParamPolicy(Policy);
    for (unsigned i = 0, e = T->getNumParams(); i != e; ++i) {
      if (i) OS << ", ";
      printQualType(T->getParamType(i), OS, StringRef());
    }
  }
  
  if (T->isVariadic()) {
    if (T->getNumParams())
      OS << ", ";
    OS << "...";
  } else if (T->getNumParams() == 0 && !Policy.LangOpts.CPlusPlus) {
    // Do not emit int() if we have a proto, emit 'int(void)'.
    OS << "void";
  }
  
  OS << ')';

  FunctionType::ExtInfo Info = T->getExtInfo();

  if (!InsideCCAttribute) {
    switch (Info.getCC()) {
    case CC_C:
      // The C calling convention is the default on the vast majority of platforms
      // we support.  If the user wrote it explicitly, it will usually be printed
      // while traversing the AttributedType.  If the type has been desugared, let
      // the canonical spelling be the implicit calling convention.
      // FIXME: It would be better to be explicit in certain contexts, such as a
      // cdecl function typedef used to declare a member function with the
      // Microsoft C++ ABI.
      break;
    case CC_X86StdCall:
      OS << " __attribute__((stdcall))";
      break;
    case CC_X86FastCall:
      OS << " __attribute__((fastcall))";
      break;
    case CC_X86ThisCall:
      OS << " __attribute__((thiscall))";
      break;
    case CC_X86Pascal:
      OS << " __attribute__((pascal))";
      break;
    case CC_AAPCS:
      OS << " __attribute__((pcs(\"aapcs\")))";
      break;
    case CC_AAPCS_VFP:
      OS << " __attribute__((pcs(\"aapcs-vfp\")))";
      break;
    case CC_PnaclCall:
      OS << " __attribute__((pnaclcall))";
      break;
    case CC_IntelOclBicc:
      OS << " __attribute__((intel_ocl_bicc))";
      break;
    case CC_X86_64Win64:
      OS << " __attribute__((ms_abi))";
      break;
    case CC_X86_64SysV:
      OS << " __attribute__((sysv_abi))";
      break;
    }
  }

  if (Info.getNoReturn())
    OS << " __attribute__((noreturn))";
  if (Info.getRegParm())
    OS << " __attribute__((regparm ("
       << Info.getRegParm() << ")))";

  if (unsigned quals = T->getTypeQuals()) {
    OS << ' ';
    AppendTypeQualList(OS, quals);
  }

  switch (T->getRefQualifier()) {
  case RQ_None:
    break;
    
  case RQ_LValue:
    OS << " &";
    break;
    
  case RQ_RValue:
    OS << " &&";
    break;
  }
  //T->printExceptionSpecification(OS, Policy);
  printExceptionSpecification(T, OS, Policy);

  if (T->hasTrailingReturn()) {
    OS << " -> ";
    printQualType(T->getReturnType(), OS, StringRef());
  } else
    printAfter(T->getReturnType(), OS);
  
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</FunctionProtoType>";
  }
}

void TypePrinter::printFunctionNoProtoBefore(const FunctionNoProtoType *T, 
                                             raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<FunctionNoProtoType>";
  }
  // If needed for precedence reasons, wrap the inner part in grouping parens.
  SaveAndRestore<bool> PrevPHIsEmpty(HasEmptyPlaceHolder, false);
  printBefore(T->getReturnType(), OS);
  if (!PrevPHIsEmpty.get())
    OS << '(';
}
void TypePrinter::printFunctionNoProtoAfter(const FunctionNoProtoType *T, 
                                            raw_ostream &OS) {
  // If needed for precedence reasons, wrap the inner part in grouping parens.
  if (!HasEmptyPlaceHolder)
    OS << ')';
  SaveAndRestore<bool> NonEmptyPH(HasEmptyPlaceHolder, false);
  
  OS << "()";
  if (T->getNoReturnAttr())
    OS << " __attribute__((noreturn))";
  printAfter(T->getReturnType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</FunctionNoProtoType>";
  }
}

void TypePrinter::printTypeSpec(const NamedDecl *D, raw_ostream &OS) {
  IdentifierInfo *II = D->getIdentifier();
  OS << II->getName();
  spaceBeforePlaceHolder(OS);
}

void TypePrinter::printUnresolvedUsingBefore(const UnresolvedUsingType *T,
                                             raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<UnresolvedUsingType>";
  }
  printTypeSpec(T->getDecl(), OS);
}
void TypePrinter::printUnresolvedUsingAfter(const UnresolvedUsingType *T,
                                             raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</UnresolvedUsingType>";
  }
}

void TypePrinter::printTypedefBefore(const TypedefType *T, raw_ostream &OS)
{ 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<TypedefType>";
  }
  printTypeSpec(T->getDecl(), OS);
}
void TypePrinter::printTypedefAfter(const TypedefType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</TypedefType>";
  }
} 

void TypePrinter::printTypeOfExprBefore(const TypeOfExprType *T,
                                        raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<TypeOfExprType>";
  }
  OS << "typeof ";
  if (T->getUnderlyingExpr())
    T->getUnderlyingExpr()->printPretty(OS, nullptr, Policy);
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printTypeOfExprAfter(const TypeOfExprType *T,
                                       raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</TypeOfExprType>";
  }
}

void TypePrinter::printTypeOfBefore(const TypeOfType *T, raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<TypeOfType>";
  }
  OS << "typeof(";
  printQualType(T->getUnderlyingType(), OS, StringRef());
  OS << ')';
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printTypeOfAfter(const TypeOfType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</TypeOfType>";
  }
} 

void TypePrinter::printDecltypeBefore(const DecltypeType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DecltypeType>";
  }
  OS << "decltype(";
  if (T->getUnderlyingExpr())
    T->getUnderlyingExpr()->printPretty(OS, nullptr, Policy);
  OS << ')';
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printDecltypeAfter(const DecltypeType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DecltypeType>";
  }
} 

void TypePrinter::printUnaryTransformBefore(const UnaryTransformType *T,
                                            raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<UnaryTransformType>";
  }
  IncludeStrongLifetimeRAII Strong(Policy);

  switch (T->getUTTKind()) {
    case UnaryTransformType::EnumUnderlyingType:
      OS << "__underlying_type(";
      printQualType(T->getBaseType(), OS, StringRef());
      OS << ')';
      spaceBeforePlaceHolder(OS);
      return;
  }

  printBefore(T->getBaseType(), OS);
}
void TypePrinter::printUnaryTransformAfter(const UnaryTransformType *T,
                                           raw_ostream &OS) {
  IncludeStrongLifetimeRAII Strong(Policy);

  switch (T->getUTTKind()) {
    case UnaryTransformType::EnumUnderlyingType:
      return;
  }

  printAfter(T->getBaseType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</UnaryTransformType>";
  }
}

void TypePrinter::printAutoBefore(const AutoType *T, raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<AutoType>";
  }
  // If the type has been deduced, do not print 'auto'.
  if (!T->getDeducedType().isNull()) {
    printBefore(T->getDeducedType(), OS);
  } else {
    OS << (T->isDecltypeAuto() ? "decltype(auto)" : "auto");
    spaceBeforePlaceHolder(OS);
  }
}
void TypePrinter::printAutoAfter(const AutoType *T, raw_ostream &OS) { 
  // If the type has been deduced, do not print 'auto'.
  if (!T->getDeducedType().isNull())
    printAfter(T->getDeducedType(), OS);

  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</AutoType>";
  }
}

void TypePrinter::printAtomicBefore(const AtomicType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<AtomicType>";
  }
  
  IncludeStrongLifetimeRAII Strong(Policy);

  OS << "_Atomic(";
  printQualType(T->getValueType(), OS, StringRef());
  OS << ')';
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printAtomicAfter(const AtomicType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</AtomicType>";
  }
}

/// Appends the given scope to the end of a string.
void TypePrinter::AppendScope(DeclContext *DC, raw_ostream &OS) {
  if (DC->isTranslationUnit()) return;
  if (DC->isFunctionOrMethod()) return;
  AppendScope(DC->getParent(), OS);

  if (NamespaceDecl *NS = dyn_cast<NamespaceDecl>(DC)) {
    if (Policy.SuppressUnwrittenScope && 
        (NS->isAnonymousNamespace() || NS->isInline()))
      return;
    if (NS->getIdentifier())
      OS << NS->getName() << "::";
    else
      OS << "(anonymous namespace)::";
  } else if (ClassTemplateSpecializationDecl *Spec
               = dyn_cast<ClassTemplateSpecializationDecl>(DC)) {
    IncludeStrongLifetimeRAII Strong(Policy);
    OS << Spec->getIdentifier()->getName();
    const TemplateArgumentList &TemplateArgs = Spec->getTemplateArgs();
    TypePrinter::PrintTemplateArgumentList(OS,
                                            TemplateArgs.data(),
                                            TemplateArgs.size(),
                                            Policy);
    OS << "::";
  } else if (TagDecl *Tag = dyn_cast<TagDecl>(DC)) {
    if (TypedefNameDecl *Typedef = Tag->getTypedefNameForAnonDecl())
      OS << Typedef->getIdentifier()->getName() << "::";
    else if (Tag->getIdentifier())
      OS << Tag->getIdentifier()->getName() << "::";
    else
      return;
  }
}

void TypePrinter::printTag(TagDecl *D, raw_ostream &OS) {
  if (Policy.SuppressTag)
    return;

  bool HasKindDecoration = false;

  // bool SuppressTagKeyword
  //   = Policy.LangOpts.CPlusPlus || Policy.SuppressTagKeyword;

  // We don't print tags unless this is an elaborated type.
  // In C, we just assume every RecordType is an elaborated type.
  if (!(Policy.LangOpts.CPlusPlus || Policy.SuppressTagKeyword ||
        D->getTypedefNameForAnonDecl())) {
    HasKindDecoration = true;
    OS << D->getKindName();
    OS << ' ';
  }

  // Compute the full nested-name-specifier for this type.
  // In C, this will always be empty except when the type
  // being printed is anonymous within other Record.
  if (!Policy.SuppressScope)
    AppendScope(D->getDeclContext(), OS);

  if (const IdentifierInfo *II = D->getIdentifier())
    OS << II->getName();
  else if (TypedefNameDecl *Typedef = D->getTypedefNameForAnonDecl()) {
    assert(Typedef->getIdentifier() && "Typedef without identifier?");
    OS << Typedef->getIdentifier()->getName();
  } else {
    // Make an unambiguous representation for anonymous types, e.g.
    //   (anonymous enum at /usr/include/string.h:120:9)
    
    if (isa<CXXRecordDecl>(D) && cast<CXXRecordDecl>(D)->isLambda()) {
      OS << "(lambda";
      HasKindDecoration = true;
    } else {
      OS << "(anonymous";
    }
    
    if (Policy.AnonymousTagLocations) {
      // Suppress the redundant tag keyword if we just printed one.
      // We don't have to worry about ElaboratedTypes here because you can't
      // refer to an anonymous type with one.
      if (!HasKindDecoration)
        OS << " " << D->getKindName();

      PresumedLoc PLoc = D->getASTContext().getSourceManager().getPresumedLoc(
          D->getLocation());
      if (PLoc.isValid()) {
        OS << " at " << PLoc.getFilename()
           << ':' << PLoc.getLine()
           << ':' << PLoc.getColumn();
      }
    }
    
    OS << ')';
  }

  // If this is a class template specialization, print the template
  // arguments.
  if (ClassTemplateSpecializationDecl *Spec
        = dyn_cast<ClassTemplateSpecializationDecl>(D)) {
    const TemplateArgument *Args;
    unsigned NumArgs;
    if (TypeSourceInfo *TAW = Spec->getTypeAsWritten()) {
      const TemplateSpecializationType *TST =
        cast<TemplateSpecializationType>(TAW->getType());
      Args = TST->getArgs();
      NumArgs = TST->getNumArgs();
    } else {
      const TemplateArgumentList &TemplateArgs = Spec->getTemplateArgs();
      Args = TemplateArgs.data();
      NumArgs = TemplateArgs.size();
    }
    IncludeStrongLifetimeRAII Strong(Policy);
    TypePrinter::PrintTemplateArgumentList(OS,
                                                          Args, NumArgs,
                                                          Policy);
  }

  spaceBeforePlaceHolder(OS);
}

void TypePrinter::printRecordBefore(const RecordType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<RecordType>";
  }
  printTag(T->getDecl(), OS);
}
void TypePrinter::printRecordAfter(const RecordType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</RecordType>";
  }
}

void TypePrinter::printEnumBefore(const EnumType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<EnumType>";
  } 
  printTag(T->getDecl(), OS);
}
void TypePrinter::printEnumAfter(const EnumType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</EnumType>";
  } 
}

void TypePrinter::printTemplateTypeParmBefore(const TemplateTypeParmType *T, 
                                              raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<TemplateTypeParmType>";
  } 
  if (IdentifierInfo *Id = T->getIdentifier())
    OS << Id->getName();
  else
    OS << "type-parameter-" << T->getDepth() << '-' << T->getIndex();
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printTemplateTypeParmAfter(const TemplateTypeParmType *T, 
                                             raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</TemplateTypeParmType>";
  } 
} 

void TypePrinter::printSubstTemplateTypeParmBefore(
                                             const SubstTemplateTypeParmType *T, 
                                             raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<SubstTemplateTypeParmType>";
  } 
  IncludeStrongLifetimeRAII Strong(Policy);
  printBefore(T->getReplacementType(), OS);
}
void TypePrinter::printSubstTemplateTypeParmAfter(
                                             const SubstTemplateTypeParmType *T, 
                                             raw_ostream &OS) { 
  IncludeStrongLifetimeRAII Strong(Policy);
  printAfter(T->getReplacementType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</SubstTemplateTypeParmType>";
  } 
}

void TypePrinter::printSubstTemplateTypeParmPackBefore(
                                        const SubstTemplateTypeParmPackType *T, 
                                        raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<SubstTemplateTypeParmPackType>";
  } 
  IncludeStrongLifetimeRAII Strong(Policy);
  printTemplateTypeParmBefore(T->getReplacedParameter(), OS);
}
void TypePrinter::printSubstTemplateTypeParmPackAfter(
                                        const SubstTemplateTypeParmPackType *T, 
                                        raw_ostream &OS) { 
  IncludeStrongLifetimeRAII Strong(Policy);
  printTemplateTypeParmAfter(T->getReplacedParameter(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</SubstTemplateTypeParmPackType>";
  } 
}

void TypePrinter::printTemplateSpecializationBefore(
                                            const TemplateSpecializationType *T, 
                                            raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<TemplateSpecializationType>";
  } 
  IncludeStrongLifetimeRAII Strong(Policy);
  T->getTemplateName().print(OS, Policy);
  
  TypePrinter::PrintTemplateArgumentList(OS,
                                                        T->getArgs(), 
                                                        T->getNumArgs(), 
                                                        Policy);
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printTemplateSpecializationAfter(
                                            const TemplateSpecializationType *T, 
                                            raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</TemplateSpecializationType>";
  }
} 

void TypePrinter::printInjectedClassNameBefore(const InjectedClassNameType *T,
                                               raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<InjectedClassNameType>";
  }
  printTemplateSpecializationBefore(T->getInjectedTST(), OS);
}
void TypePrinter::printInjectedClassNameAfter(const InjectedClassNameType *T,
                                               raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</InjectedClassNameType>";
  }
}

void TypePrinter::printElaboratedBefore(const ElaboratedType *T,
                                        raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ElaboratedType>";
  }
  if (Policy.SuppressTag && isa<TagType>(T->getNamedType()))
    return;
  OS << TypeWithKeyword::getKeywordName(T->getKeyword());
  if (T->getKeyword() != ETK_None)
    OS << " ";
  NestedNameSpecifier* Qualifier = T->getQualifier();
  if (Qualifier)
     printNestedNameSpecifier(Qualifier, OS, Policy);
    //Qualifier->print(OS, Policy);
  
  ElaboratedTypePolicyRAII PolicyRAII(Policy);
  printBefore(T->getNamedType(), OS);
}
void TypePrinter::printElaboratedAfter(const ElaboratedType *T,
                                        raw_ostream &OS) {
  ElaboratedTypePolicyRAII PolicyRAII(Policy);
  printAfter(T->getNamedType(), OS);
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ElaboratedType>";
  }
}

void TypePrinter::printParenBefore(const ParenType *T, raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ParenType>";
  }
  if (!HasEmptyPlaceHolder && !isa<FunctionType>(T->getInnerType())) {
    printBefore(T->getInnerType(), OS);
    OS << '(';
  } else
    printBefore(T->getInnerType(), OS);
}
void TypePrinter::printParenAfter(const ParenType *T, raw_ostream &OS) {
  if (!HasEmptyPlaceHolder && !isa<FunctionType>(T->getInnerType())) {
    OS << ')';
    printAfter(T->getInnerType(), OS);
  } else
    printAfter(T->getInnerType(), OS);
  
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ParenType>";
  }
}

void TypePrinter::printDependentNameBefore(const DependentNameType *T,
                                           raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DependentNameType>";
  }
  OS << TypeWithKeyword::getKeywordName(T->getKeyword());
  if (T->getKeyword() != ETK_None)
    OS << " ";
  
  //T->getQualifier()->print(OS, Policy);
  printNestedNameSpecifier(T->getQualifier(), OS, Policy);
  
  OS << T->getIdentifier()->getName();
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printDependentNameAfter(const DependentNameType *T,
                                          raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DependentNameType>";
  }
} 

void TypePrinter::printDependentTemplateSpecializationBefore(
        const DependentTemplateSpecializationType *T, raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<DependentTemplateSpecializationType>";
  }
  
  IncludeStrongLifetimeRAII Strong(Policy);
  
  OS << TypeWithKeyword::getKeywordName(T->getKeyword());
  if (T->getKeyword() != ETK_None)
    OS << " ";
  
  if (T->getQualifier())
    printNestedNameSpecifier(T->getQualifier(), OS, Policy);
    //T->getQualifier()->print(OS, Policy);    
  OS << T->getIdentifier()->getName();
  TypePrinter::PrintTemplateArgumentList(OS,
                                                        T->getArgs(),
                                                        T->getNumArgs(),
                                                        Policy);
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printDependentTemplateSpecializationAfter(
        const DependentTemplateSpecializationType *T, raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</DependentTemplateSpecializationType>";
  }
} 

void TypePrinter::printPackExpansionBefore(const PackExpansionType *T, 
                                           raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<PackExpansionType>";
  }
  printBefore(T->getPattern(), OS);
}
void TypePrinter::printPackExpansionAfter(const PackExpansionType *T, 
                                          raw_ostream &OS) {
  printAfter(T->getPattern(), OS);
  OS << "...";
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</PackExpansionType>";
  }
}

void TypePrinter::printAttributedBefore(const AttributedType *T,
                                        raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<AttributedType>";
  }
  // Prefer the macro forms of the GC and ownership qualifiers.
  if (T->getAttrKind() == AttributedType::attr_objc_gc ||
      T->getAttrKind() == AttributedType::attr_objc_ownership)
    return printBefore(T->getEquivalentType(), OS);

  printBefore(T->getModifiedType(), OS);

  if (T->isMSTypeSpec()) {
    switch (T->getAttrKind()) {
    default: return;
    case AttributedType::attr_ptr32: OS << " __ptr32"; break;
    case AttributedType::attr_ptr64: OS << " __ptr64"; break;
    case AttributedType::attr_sptr: OS << " __sptr"; break;
    case AttributedType::attr_uptr: OS << " __uptr"; break;
    }
    spaceBeforePlaceHolder(OS);
  }
}

void TypePrinter::printAttributedAfter(const AttributedType *T,
                                       raw_ostream &OS) {
  // Prefer the macro forms of the GC and ownership qualifiers.
  if (T->getAttrKind() == AttributedType::attr_objc_gc ||
      T->getAttrKind() == AttributedType::attr_objc_ownership)
    {
      printAfter(T->getEquivalentType(), OS);
      if(Policy.SuppressTypeTag == false)
      {
  	OS<<"</AttributedType>";
      }
      return;
    }

  // TODO: not all attributes are GCC-style attributes.
  if (T->isMSTypeSpec())
  {
    if(Policy.SuppressTypeTag == false)
    {
  	OS<<"</AttributedType>";
    }
    return;
  }

  // If this is a calling convention attribute, don't print the implicit CC from
  // the modified type.
  SaveAndRestore<bool> MaybeSuppressCC(InsideCCAttribute, T->isCallingConv());

  printAfter(T->getModifiedType(), OS);

  OS << " __attribute__((";
  switch (T->getAttrKind()) {
  default: llvm_unreachable("This attribute should have been handled already");
  case AttributedType::attr_address_space:
    OS << "address_space(";
    OS << T->getEquivalentType().getAddressSpace();
    OS << ')';
    break;

  case AttributedType::attr_vector_size: {
    OS << "__vector_size__(";
    if (const VectorType *vector =T->getEquivalentType()->getAs<VectorType>()) {
      OS << vector->getNumElements();
      OS << " * sizeof(";
      printQualType(vector->getElementType(), OS, StringRef());
      OS << ')';
    }
    OS << ')';
    break;
  }

  case AttributedType::attr_neon_vector_type:
  case AttributedType::attr_neon_polyvector_type: {
    if (T->getAttrKind() == AttributedType::attr_neon_vector_type)
      OS << "neon_vector_type(";
    else
      OS << "neon_polyvector_type(";
    const VectorType *vector = T->getEquivalentType()->getAs<VectorType>();
    OS << vector->getNumElements();
    OS << ')';
    break;
  }

  case AttributedType::attr_regparm: {
    // FIXME: When Sema learns to form this AttributedType, avoid printing the
    // attribute again in printFunctionProtoAfter.
    OS << "regparm(";
    QualType t = T->getEquivalentType();
    while (!t->isFunctionType())
      t = t->getPointeeType();
    OS << t->getAs<FunctionType>()->getRegParmType();
    OS << ')';
    break;
  }

  case AttributedType::attr_objc_gc: {
    OS << "objc_gc(";

    QualType tmp = T->getEquivalentType();
    while (tmp.getObjCGCAttr() == Qualifiers::GCNone) {
      QualType next = tmp->getPointeeType();
      if (next == tmp) break;
      tmp = next;
    }

    if (tmp.isObjCGCWeak())
      OS << "weak";
    else
      OS << "strong";
    OS << ')';
    break;
  }

  case AttributedType::attr_objc_ownership:
    OS << "objc_ownership(";
    switch (T->getEquivalentType().getObjCLifetime()) {
    case Qualifiers::OCL_None: llvm_unreachable("no ownership!");
    case Qualifiers::OCL_ExplicitNone: OS << "none"; break;
    case Qualifiers::OCL_Strong: OS << "strong"; break;
    case Qualifiers::OCL_Weak: OS << "weak"; break;
    case Qualifiers::OCL_Autoreleasing: OS << "autoreleasing"; break;
    }
    OS << ')';
    break;

  // FIXME: When Sema learns to form this AttributedType, avoid printing the
  // attribute again in printFunctionProtoAfter.
  case AttributedType::attr_noreturn: OS << "noreturn"; break;

  case AttributedType::attr_cdecl: OS << "cdecl"; break;
  case AttributedType::attr_fastcall: OS << "fastcall"; break;
  case AttributedType::attr_stdcall: OS << "stdcall"; break;
  case AttributedType::attr_thiscall: OS << "thiscall"; break;
  case AttributedType::attr_pascal: OS << "pascal"; break;
  case AttributedType::attr_ms_abi: OS << "ms_abi"; break;
  case AttributedType::attr_sysv_abi: OS << "sysv_abi"; break;
  case AttributedType::attr_pcs:
  case AttributedType::attr_pcs_vfp: {
    OS << "pcs(";
   QualType t = T->getEquivalentType();
   while (!t->isFunctionType())
     t = t->getPointeeType();
   OS << (t->getAs<FunctionType>()->getCallConv() == CC_AAPCS ?
         "\"aapcs\"" : "\"aapcs-vfp\"");
   OS << ')';
   break;
  }
  case AttributedType::attr_pnaclcall: OS << "pnaclcall"; break;
  case AttributedType::attr_inteloclbicc: OS << "inteloclbicc"; break;
  }
  OS << "))";
  
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</AttributedType>";
  }
}

void TypePrinter::printObjCInterfaceBefore(const ObjCInterfaceType *T, 
                                           raw_ostream &OS) { 
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ObjCInterfaceType>";
  }
  OS << T->getDecl()->getName();
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printObjCInterfaceAfter(const ObjCInterfaceType *T, 
                                          raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ObjCInterfaceType>";
  }
} 

void TypePrinter::printObjCObjectBefore(const ObjCObjectType *T,
                                        raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ObjCObjectType>";
  }
  if (T->qual_empty())
    return printBefore(T->getBaseType(), OS);

  printQualType(T->getBaseType(), OS, StringRef());
  OS << '<';
  bool isFirst = true;
  for (const auto *I : T->quals()) {
    if (isFirst)
      isFirst = false;
    else
      OS << ',';
    OS << I->getName();
  }
  OS << '>';
  spaceBeforePlaceHolder(OS);
}
void TypePrinter::printObjCObjectAfter(const ObjCObjectType *T,
                                        raw_ostream &OS) {
  if (T->qual_empty())
  {
    printAfter(T->getBaseType(), OS);
  }
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ObjCObjectType>";
  }
}

void TypePrinter::printObjCObjectPointerBefore(const ObjCObjectPointerType *T, 
                                               raw_ostream &OS) {
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"<ObjCObjectPointerType>";
  }
  
  //T->getPointeeType().getLocalQualifiers().print(OS, Policy,
  //                                              /*appendSpaceIfNonEmpty=*/true);
  printQualifiers(T->getPointeeType().getLocalQualifiers(), OS, Policy, true);
  
  assert(!T->isObjCSelType());

  if (T->isObjCIdType() || T->isObjCQualifiedIdType())
    OS << "id";
  else if (T->isObjCClassType() || T->isObjCQualifiedClassType())
    OS << "Class";
  else
    OS << T->getInterfaceDecl()->getName();
  
  if (!T->qual_empty()) {
    OS << '<';
    for (ObjCObjectPointerType::qual_iterator I = T->qual_begin(), 
                                              E = T->qual_end();
         I != E; ++I) {
      OS << (*I)->getName();
      if (I+1 != E)
        OS << ',';
    }
    OS << '>';
  }
  
  if (!T->isObjCIdType() && !T->isObjCQualifiedIdType() &&
      !T->isObjCClassType() && !T->isObjCQualifiedClassType()) {
    OS << " *"; // Don't forget the implicit pointer.
  } else {
    spaceBeforePlaceHolder(OS);
  }
}
void TypePrinter::printObjCObjectPointerAfter(const ObjCObjectPointerType *T, 
                                              raw_ostream &OS)
{
  if(Policy.SuppressTypeTag == false)
  {
  	OS<<"</ObjCObjectPointerType>";
  }
}


}//namespace another
