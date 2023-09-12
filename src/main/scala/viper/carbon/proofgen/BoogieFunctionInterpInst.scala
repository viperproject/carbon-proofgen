package viper.carbon.proofgen


trait BoogieFunId
{
  def isaRepr : String
  def funWfThm : String
}

case object GoodStateBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FGoodState"

  override def funWfThm: String = "good_state_fun_interp_single_wf"
}
case object GoodMaskBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FGoodMask"

  override def funWfThm: String = "good_mask_fun_interp_single_wf"
}
case object ReadHeapBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FReadHeap"

  override def funWfThm: String = "select_heap_fun_interp_single_wf"
}
case object UpdateHeapBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FUpdateHeap"

  override def funWfThm: String = "store_heap_fun_interp_single_wf"
}
case object ReadMaskBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FReadMask"

  override def funWfThm: String = "select_mask_fun_interp_single_wf"
}
case object UpdateMaskBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FUpdateMask"

  override def funWfThm: String = "store_mask_fun_interp_single_wf"
}
case object HasPermBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FHasPerm"

  override def funWfThm: String = "has_direct_perm_fun_interp_single_wf"
}
case object IdenticalOnKnownLocsBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FIdenticalOnKnownLocs"

  override def funWfThm: String = "identical_on_known_locs_fun_interp_single_wf"
}
case object IsPredicateFieldBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FIsPredicateField"

  override def funWfThm: String = "is_predicate_field_fun_interp_single_wf"
}
case object IsWandFieldBoogieFun extends BoogieFunId {
  override def isaRepr: String = "FIsWandField"

  override def funWfThm: String = "is_wand_field_fun_interp_single_wf"
}
