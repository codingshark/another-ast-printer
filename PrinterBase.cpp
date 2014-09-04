#include "PrinterBase.h"

#include "TypePrinter.h"

namespace another_printer
{

std::string PrinterBase::QualTypeGetAsString(QualType tp, const another_printer::PrintingPolicy &Policy) const
{
  std::string S;
  QualTypeGetAsStringInternal(tp, S, Policy);
  return S;
}

/*std::string PrinterBase::QualTypeGetAsString(const Type *ty, Qualifiers qs) 
{
  std::string buffer;
  LangOptions options;
  QualTypeGetAsStringInternal(ty, qs, buffer, PrintingPolicy(options));
  return buffer;
}*/

void PrinterBase::QualTypeGetAsStringInternal(QualType & tp, std::string &Str, const another_printer::PrintingPolicy &Policy) const
{
     return QualTypeGetAsStringInternal(tp.split(), Str, Policy);
}

void PrinterBase::QualTypeGetAsStringInternal(SplitQualType split, std::string &out, const another_printer::PrintingPolicy &policy) const
{
     return QualTypeGetAsStringInternal(split.Ty, split.Quals, out, policy);
}

void PrinterBase::QualTypeGetAsStringInternal(const Type *ty, Qualifiers qs,
                                   std::string &buffer,
                                   const another_printer::PrintingPolicy &policy) const
{
  SmallString<256> Buf;
  llvm::raw_svector_ostream StrOS(Buf);
  another_printer::TypePrinter(policy).print(ty, qs, StrOS, buffer);
  std::string str = StrOS.str();
  buffer.swap(str);
}

}//another_printer


