use crate::postparsing::names::{INameS, IVarNameS};
use crate::postparsing::post_parser::ICompileErrorS;
use crate::utils::range::{CodeLocationS, RangeS};
use crate::postparsing::names::IImpreciseNameS;
use crate::postparsing::names::IRuneS;
use crate::postparsing::rules::rules::IRulexSR;
use crate::postparsing::itemplatatype::ITemplataType;
use crate::postparsing::rules::rules::ILiteralSL;
use crate::parsing::ast::MutabilityP;
use crate::parsing::ast::VariabilityP;
use crate::parsing::ast::OwnershipP;
use crate::postparsing::rules::rules::RuneUsage;
use crate::postparsing::rune_type_solver::IRuneTypeRuleError;


pub fn humanize<'s, HP, LB, LRC, LC>(
  humanize_pos: HP,
  _lines_between: LB,
  _line_range_containing: LRC,
  line_containing: LC,
  err: &'s ICompileErrorS<'s>,
) -> String
where
  HP: Fn(&CodeLocationS<'s>) -> String,
  LB: Fn(&CodeLocationS<'s>, &CodeLocationS<'s>) -> Vec<RangeS<'s>>,
  LRC: Fn(&CodeLocationS<'s>) -> RangeS<'s>,
  LC: Fn(&CodeLocationS<'s>) -> String,
{
  let error_str_body = match err {
    ICompileErrorS::VariableNameAlreadyExists(x) => {
      format!(
        "Local named {} already exists!\n(If you meant to modify the variable, use the `set` keyword beforehand.)",
        humanize_var_name(x.name.clone())
      )
    }
    ICompileErrorS::InterfaceMethodNeedsSelf(_) => {
      "Interface's method needs a virtual param of interface's type!".to_string()
    }
    ICompileErrorS::VirtualAndAbstractGoTogether(_) => {
      "Abstract function needs a `virtual` parameter.".to_string()
    }
    ICompileErrorS::ExternHasBodyS(_) => "Extern function can't have a body too.".to_string(),
    ICompileErrorS::IdentifyingRunesIncompleteS(_) => {
      "Not enough identifying runes.".to_string()
    }
    _ => panic!("Unimplemented humanize branch for {:?}", err),
  };
  let range = err.range();
  let pos_str = humanize_pos(&range.begin);
  let next_stuff = line_containing(&range.begin);
  let error_id = "S";
  format!("{} error {}: {}\n{}\n", pos_str, error_id, error_str_body, next_stuff)
}

pub fn humanize_rune_type_error<'s>(
  _code_map: &dyn Fn(CodeLocationS<'s>) -> String,
  error: &IRuneTypeRuleError<'s>,
) -> String {
  match error {
    IRuneTypeRuleError::FoundTemplataDidntMatchExpectedType(_) => panic!("implement: humanize_rune_type_error FoundTemplataDidntMatchExpectedType"),
    IRuneTypeRuleError::CouldntFindType(e) => {
      format!("Couldn't find anything with the name '{}'", humanize_imprecise_name(e.name))
    }
    IRuneTypeRuleError::NotEnoughArgumentsForGenericCall(_) => panic!("implement: humanize_rune_type_error NotEnoughArgumentsForGenericCall"),
    _ => panic!("implement: humanize_rune_type_error other"),
  }
}

fn humanize_identifiability_rule_errorr<'s>(
  _error: &(),
) -> String {
  panic!("Unimplemented humanize_identifiability_rule_errorr");
}

fn humanize_var_name<'s>(var_name: IVarNameS<'s>) -> String {
  match var_name {
    IVarNameS::CodeVarName(n) => n.as_str().to_string(),
    IVarNameS::ClosureParamName(_) => "(closure)".to_string(),
    _ => panic!("Unimplemented humanize_var_name branch for IVarNameS"),
  }
}

fn humanize_name<'s>(name: INameS<'s>) -> String {
  match name {
    INameS::VarName(var_name) => humanize_var_name((*var_name).clone()),
    _ => panic!("Unimplemented humanize_name branch for INameS"),
  }
}

pub fn humanize_imprecise_name<'s>(
  name: IImpreciseNameS<'s>,
) -> String {
  match name {
    IImpreciseNameS::ArbitraryName(_) => "_arby".to_string(),
    IImpreciseNameS::SelfName(_) => "_Self".to_string(),
    IImpreciseNameS::CodeName(n) => n.name.0.to_string(),
    IImpreciseNameS::RuneName(rune) => humanize_rune(rune.rune),
    IImpreciseNameS::AnonymousSubstructTemplateImpreciseName(_) => panic!("implement: humanize_imprecise_name AnonymousSubstructTemplateImpreciseName"),
    IImpreciseNameS::LambdaStructImpreciseName(_) => panic!("implement: humanize_imprecise_name LambdaStructImpreciseName"),
    IImpreciseNameS::LambdaImpreciseName(_) => "_Lam".to_string(),
    _ => panic!("implement: humanize_imprecise_name other"),
  }
}

pub fn humanize_rune<'s>(
  rune: IRuneS<'s>,
) -> String {
  match rune {
    IRuneS::ImplicitRune(r) => "_".to_string() + &r.lid.path.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(""),
    IRuneS::MagicParamRune(_) => panic!("implement: humanize_rune MagicParamRune"),
    IRuneS::CodeRune(r) => r.name.0.to_string(),
    IRuneS::ArgumentRune(_) => panic!("implement: humanize_rune ArgumentRune"),
    IRuneS::SelfKindRune(_) => panic!("implement: humanize_rune SelfKindRune"),
    IRuneS::SelfOwnershipRune(_) => panic!("implement: humanize_rune SelfOwnershipRune"),
    IRuneS::SelfKindTemplateRune(_) => panic!("implement: humanize_rune SelfKindTemplateRune"),
    IRuneS::PatternInputRune(_) => panic!("implement: humanize_rune PatternInputRune"),
    IRuneS::SelfRune(_) => panic!("implement: humanize_rune SelfRune"),
    IRuneS::SelfCoordRune(_) => panic!("implement: humanize_rune SelfCoordRune"),
    IRuneS::ReturnRune(_) => panic!("implement: humanize_rune ReturnRune"),
    IRuneS::AnonymousSubstructParentInterfaceTemplateRune(_) => panic!("implement: humanize_rune AnonymousSubstructParentInterfaceTemplateRune"),
    IRuneS::ImplDropVoidRune(_) => panic!("implement: humanize_rune ImplDropVoidRune"),
    IRuneS::ImplDropCoordRune(_) => panic!("implement: humanize_rune ImplDropCoordRune"),
    IRuneS::FreeOverrideInterfaceRune(_) => panic!("implement: humanize_rune FreeOverrideInterfaceRune"),
    IRuneS::FreeOverrideStructRune(_) => panic!("implement: humanize_rune FreeOverrideStructRune"),
    IRuneS::AnonymousSubstructKindRune(_) => panic!("implement: humanize_rune AnonymousSubstructKindRune"),
    IRuneS::AnonymousSubstructCoordRune(_) => panic!("implement: humanize_rune AnonymousSubstructCoordRune"),
    IRuneS::AnonymousSubstructTemplateRune(_) => panic!("implement: humanize_rune AnonymousSubstructTemplateRune"),
    IRuneS::AnonymousSubstructParentInterfaceKindRune(_) => panic!("implement: humanize_rune AnonymousSubstructParentInterfaceKindRune"),
    IRuneS::AnonymousSubstructParentInterfaceCoordRune(_) => panic!("implement: humanize_rune AnonymousSubstructParentInterfaceCoordRune"),
    IRuneS::StructNameRune(_) => panic!("implement: humanize_rune StructNameRune"),
    IRuneS::FreeOverrideStructTemplateRune(_) => panic!("implement: humanize_rune FreeOverrideStructTemplateRune"),
    IRuneS::FunctorPrototypeRuneName(_) => panic!("implement: humanize_rune FunctorPrototypeRuneName"),
    IRuneS::MacroSelfKindRune(_) => panic!("implement: humanize_rune MacroSelfKindRune"),
    IRuneS::MacroSelfCoordRune(_) => panic!("implement: humanize_rune MacroSelfCoordRune"),
    IRuneS::MacroVoidKindRune(_) => panic!("implement: humanize_rune MacroVoidKindRune"),
    IRuneS::MacroVoidCoordRune(_) => panic!("implement: humanize_rune MacroVoidCoordRune"),
    IRuneS::MacroSelfKindTemplateRune(_) => panic!("implement: humanize_rune MacroSelfKindTemplateRune"),
    IRuneS::AnonymousSubstructMemberRune(_) => panic!("implement: humanize_rune AnonymousSubstructMemberRune"),
    IRuneS::AnonymousSubstructFunctionBoundParamsListRune(_) => panic!("implement: humanize_rune AnonymousSubstructFunctionBoundParamsListRune"),
    IRuneS::AnonymousSubstructFunctionBoundPrototypeRune(_) => panic!("implement: humanize_rune AnonymousSubstructFunctionBoundPrototypeRune"),
    IRuneS::AnonymousSubstructFunctionInterfaceTemplateRune(_) => panic!("implement: humanize_rune AnonymousSubstructFunctionInterfaceTemplateRune"),
    IRuneS::AnonymousSubstructFunctionInterfaceKindRune(_) => panic!("implement: humanize_rune AnonymousSubstructFunctionInterfaceKindRune"),
    IRuneS::AnonymousSubstructDropBoundParamsListRune(_) => panic!("implement: humanize_rune AnonymousSubstructDropBoundParamsListRune"),
    IRuneS::AnonymousSubstructDropBoundPrototypeRune(_) => panic!("implement: humanize_rune AnonymousSubstructDropBoundPrototypeRune"),
    IRuneS::AnonymousSubstructMethodInheritedRune(_) => panic!("implement: humanize_rune AnonymousSubstructMethodInheritedRune"),
    IRuneS::AnonymousSubstructMethodSelfOwnCoordRune(_) => panic!("implement: humanize_rune AnonymousSubstructMethodSelfOwnCoordRune"),
    IRuneS::AnonymousSubstructMethodSelfBorrowCoordRune(_) => panic!("implement: humanize_rune AnonymousSubstructMethodSelfBorrowCoordRune"),
    IRuneS::DenizenDefaultRegionRune(_) => panic!("implement: humanize_rune DenizenDefaultRegionRune"),
    IRuneS::ExternDefaultRegionRune(_) => panic!("implement: humanize_rune ExternDefaultRegionRune"),
    IRuneS::AnonymousSubstructVoidKindRune(_) => panic!("implement: humanize_rune AnonymousSubstructVoidKindRune"),
    IRuneS::AnonymousSubstructVoidCoordRune(_) => panic!("implement: humanize_rune AnonymousSubstructVoidCoordRune"),
    IRuneS::ImplicitCoercionOwnershipRune(_) => panic!("implement: humanize_rune ImplicitCoercionOwnershipRune"),
    IRuneS::ImplicitCoercionKindRune(_) => panic!("implement: humanize_rune ImplicitCoercionKindRune"),
    IRuneS::ImplicitCoercionTemplateRune(_) => panic!("implement: humanize_rune ImplicitCoercionTemplateRune"),
    IRuneS::ImplicitRegionRune(_) => panic!("implement: humanize_rune ImplicitRegionRune"),
    IRuneS::CallRegionRune(_) => panic!("implement: humanize_rune CallRegionRune"),
    IRuneS::CaseRuneFromImpl(_) => panic!("implement: humanize_rune CaseRuneFromImpl"),
    IRuneS::DispatcherRuneFromImpl(_) => panic!("implement: humanize_rune DispatcherRuneFromImpl"),
    IRuneS::CallPureMergeRegionRune(_) => panic!("implement: humanize_rune CallPureMergeRegionRune"),
    IRuneS::ReachablePrototypeRune(_) => panic!("implement: humanize_rune ReachablePrototypeRune"),
    IRuneS::MemberRune(_) => panic!("implement: humanize_rune MemberRune"),
    IRuneS::LocalDefaultRegionRune(_) => panic!("implement: humanize_rune LocalDefaultRegionRune"),
    IRuneS::ExportDefaultRegionRune(_) => panic!("implement: humanize_rune ExportDefaultRegionRune"),
    IRuneS::ArraySizeImplicitRune(_) => panic!("implement: humanize_rune ArraySizeImplicitRune"),
    IRuneS::ArrayMutabilityImplicitRune(_) => panic!("implement: humanize_rune ArrayMutabilityImplicitRune"),
    IRuneS::ArrayVariabilityImplicitRune(_) => panic!("implement: humanize_rune ArrayVariabilityImplicitRune"),
    IRuneS::InterfaceNameRune(_) => panic!("implement: humanize_rune InterfaceNameRune"),
    IRuneS::LetImplicitRune(_) => panic!("implement: humanize_rune LetImplicitRune"),
    IRuneS::ExplicitTemplateArgRune(_) => panic!("implement: humanize_rune ExplicitTemplateArgRune"),
    IRuneS::FunctorParamRuneName(_) => panic!("implement: humanize_rune FunctorParamRuneName"),
    IRuneS::FunctorReturnRuneName(_) => panic!("implement: humanize_rune FunctorReturnRuneName"),
  }
}

pub fn humanize_templata_type(
  _tyype: &ITemplataType,
) -> String {
  panic!("Unimplemented humanize_templata_type");
}

pub fn humanize_rule<'s>(
  rule: &IRulexSR<'s>,
) -> String {
  match rule {
    IRulexSR::KindComponents(r) => {
      humanize_rune(r.kind_rune.rune) + " = Kind[" + &humanize_rune(r.mutability_rune.rune) + "]"
    }
    IRulexSR::CoordComponents(r) => {
      humanize_rune(r.result_rune.rune) + " = Ref[" + &humanize_rune(r.ownership_rune.rune) + ", " + &humanize_rune(r.kind_rune.rune) + "]"
    }
    IRulexSR::PrototypeComponents(_) => panic!("implement: humanize_rule PrototypeComponents"),
    IRulexSR::OneOf(_) => panic!("implement: humanize_rule OneOf"),
    IRulexSR::IsInterface(_) => panic!("implement: humanize_rule IsInterface"),
    IRulexSR::IsStruct(_) => panic!("implement: humanize_rule IsStruct"),
    IRulexSR::RefListCompoundMutability(_) => panic!("implement: humanize_rule RefListCompoundMutability"),
    IRulexSR::DefinitionCoordIsa(_) => panic!("implement: humanize_rule DefinitionCoordIsa"),
    IRulexSR::CallSiteCoordIsa(_) => panic!("implement: humanize_rule CallSiteCoordIsa"),
    IRulexSR::CoordSend(_) => panic!("implement: humanize_rule CoordSend"),
    IRulexSR::CoerceToCoord(_) => panic!("implement: humanize_rule CoerceToCoord"),
    IRulexSR::MaybeCoercingCall(r) => humanize_rune(r.result_rune.rune) + " = " + &humanize_rune(r.template_rune.rune) + "<" + &r.args.iter().map(|x| humanize_rune(x.rune)).collect::<Vec<_>>().join(", ") + ">",
    IRulexSR::MaybeCoercingLookup(r) => humanize_rune(r.rune.rune) + " = \"" + &humanize_imprecise_name(r.name) + "\"",
    IRulexSR::Call(_) => panic!("implement: humanize_rule Call"),
    IRulexSR::Lookup(_) => panic!("implement: humanize_rule Lookup"),
    IRulexSR::Literal(r) => humanize_rune(r.rune.rune) + " = " + &humanize_literal(&r.literal),
    IRulexSR::Augment(r) => humanize_rune(r.result_rune.rune) + " = " + &r.ownership.map(humanize_ownership).unwrap_or_else(String::new) + &humanize_rune(r.inner_rune.rune),
    IRulexSR::Equals(_) => panic!("implement: humanize_rule Equals"),
    IRulexSR::RuneParentEnvLookup(_) => panic!("implement: humanize_rule RuneParentEnvLookup"),
    IRulexSR::Pack(_) => panic!("implement: humanize_rule Pack"),
    IRulexSR::Resolve(_) => panic!("implement: humanize_rule Resolve"),
    IRulexSR::CallSiteFunc(_) => panic!("implement: humanize_rule CallSiteFunc"),
    IRulexSR::DefinitionFunc(_) => panic!("implement: humanize_rule DefinitionFunc"),
    other => panic!("vimpl humanize_rule: {:?}", other),
  }
}

fn humanize_literal(
  literal: &ILiteralSL,
) -> String {
  match literal {
    ILiteralSL::OwnershipLiteral(_) => panic!("Unimplemented: humanize_literal OwnershipLiteral"),
    ILiteralSL::MutabilityLiteral(x) => humanize_mutability(x.mutability),
    ILiteralSL::VariabilityLiteral(_) => panic!("Unimplemented: humanize_literal VariabilityLiteral"),
    ILiteralSL::IntLiteral(x) => x.value.to_string(),
    ILiteralSL::StringLiteral(_) => panic!("Unimplemented: humanize_literal StringLiteral"),
    other => panic!("vimpl humanize_literal: {:?}", other),
  }
}

fn humanize_mutability(
  p: MutabilityP,
) -> String {
  match p {
    MutabilityP::Mutable => "mut".to_string(),
    MutabilityP::Immutable => "imm".to_string(),
  }
}

fn humanize_variability(
  _p: VariabilityP,
) -> String {
  panic!("Unimplemented humanize_variability");
}

fn humanize_ownership(
  p: OwnershipP,
) -> String {
  match p {
    OwnershipP::Own => "^".to_string(),
    OwnershipP::Share => "@".to_string(),
    OwnershipP::Borrow => "&".to_string(),
    OwnershipP::Weak => "&&".to_string(),
    OwnershipP::Live => panic!("Unimplemented: humanize_ownership Live"),
  }
}

fn humanize_region<'s>(
  _r: &RuneUsage<'s>,
) -> String {
  panic!("Unimplemented humanize_region");
}
