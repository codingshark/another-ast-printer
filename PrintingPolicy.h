#ifndef _PRINTING_POLICY_
#define _PRINTING_POLICY_

#include "clang/AST/PrettyPrinter.h"

namespace another_printer{

class PrintingPolicy: public clang::PrintingPolicy
{
	public:
		PrintingPolicy(const clang::PrintingPolicy & a_policy,
					bool a_suppress_decl_tag_flag,
					bool a_suppress_type_tag_flag,
					bool a_suppress_stmt_tag_flag,
					bool a_suppress_omp_tag_flag,
					bool a_suppress_expr_tag_flag):
		clang::PrintingPolicy(a_policy),
		SuppressDeclTag(a_suppress_decl_tag_flag),
		SuppressTypeTag(a_suppress_type_tag_flag),
		SuppressStmtTag(a_suppress_stmt_tag_flag),
		SuppressOMPTag(a_suppress_omp_tag_flag),
		SuppressExprTag(a_suppress_expr_tag_flag){};
		
		PrintingPolicy(const another_printer::PrintingPolicy & a_policy):
		clang::PrintingPolicy(a_policy),
		SuppressDeclTag(a_policy.SuppressDeclTag),
		SuppressTypeTag(a_policy.SuppressTypeTag),
		SuppressStmtTag(a_policy.SuppressStmtTag),
		SuppressOMPTag(a_policy.SuppressOMPTag),
		SuppressExprTag(a_policy.SuppressExprTag){};

		virtual ~PrintingPolicy(){};

		bool SuppressDeclTag : 1;
		bool SuppressTypeTag : 1;
		bool SuppressStmtTag : 1;
		bool SuppressOMPTag : 1;
		bool SuppressExprTag : 1;
};

}

#endif
