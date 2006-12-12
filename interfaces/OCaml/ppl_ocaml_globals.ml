type mip_problem

external ppl_new_MIP_Problem_from_space_dimension:
  int -> mip_problem = "ppl_new_MIP_Problem_from_space_dimension"

external ppl_MIP_Problem_space_dimension:
  mip_problem -> int = "ppl_MIP_Problem_space_dimension"
