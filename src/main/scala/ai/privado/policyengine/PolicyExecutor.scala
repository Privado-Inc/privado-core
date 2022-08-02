package ai.privado.policyengine

import ai.privado.cache.RuleCache
import ai.privado.model.{Policy, PolicyAction}

object PolicyExecutor {

  val ALL_MATCH = "**"
  val actionMap = Map(PolicyAction.ALLOW -> false, PolicyAction.DENY -> true)

  def execute(repoName: String = "accounts-j") = {
    println("printing policies")
    println(RuleCache.getAllPolicy)
    println(RuleCache.getAllPolicy.size)
    val policies = RuleCache.getAllPolicy.filter(policy => filterByRepoName(policy, repoName))
    println("printing filtered policies")
    println(policies)
    println(policies.size)
  }

  /*
  Filters outs based on Repository name
   */
  private def filterByRepoName(policy: Policy, repoName: String) = {
    policy.repositories
      .flatMap(repoPattern => {
        if (repoPattern.equals(ALL_MATCH) || repoPattern.matches(repoName)) {
          // println(s"match found ${policy.id}, ${policy.file}")
          Some(true)
        } else {
          // println(s"match not found ${policy.id}, ${policy.file}")
          Some(false)
        }
      })
      .reduce((a, b) => a & b) || actionMap.getOrElse(policy.action, true)
  }
}
