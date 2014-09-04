#ifndef _PRINTER_BASE_
#define _PRINTER_BASE_

#include "llvm/Support/raw_ostream.h"
#include "clang/AST/Type.h"
#include "PrintingPolicy.h"
#include "llvm/ADT/SmallString.h"

using namespace clang;

namespace another_printer
{

class PrinterBase
{
	public:
	std::string QualTypeGetAsString(QualType tp, const another_printer::PrintingPolicy &Policy) const;
	//std::string QualTypeGetAsString(const Type *ty, Qualifiers qs);
	
	protected:
	void QualTypeGetAsStringInternal(QualType & tp, std::string &Str, const another_printer::PrintingPolicy &Policy) const;
	void QualTypeGetAsStringInternal(SplitQualType split, std::string &out, const another_printer::PrintingPolicy &policy) const;
	void QualTypeGetAsStringInternal(const Type *ty, Qualifiers qs,
                                   std::string &buffer,
                                   const another_printer::PrintingPolicy &policy) const;
};

}

#endif
