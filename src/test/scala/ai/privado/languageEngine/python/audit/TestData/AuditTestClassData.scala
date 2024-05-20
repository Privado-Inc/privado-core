package ai.privado.languageEngine.python.audit.TestData

object AuditTestClassData {
  val user: String = """
      |class User:
      |  def __init__(self, firstName):
      |    self.firstName = firstName
      |
      |  def getFirstName(self):
      |    return self.firstName
      |  
      |  def setFirstName(self, firstName):
      |    self.firstName = firstName
      |""".stripMargin

  val account: String = """
      |class Account:
      |  def __init__(self, accountNo):
      |    self.accountNo = accountNo
      |
      |  def setAccountNo(self, accountNo):
      |    self.accountNo = accountNo
      |""".stripMargin

  val address: String = """
      |class Address:
      |  def __init__(self, houseNo):
      |    self.houseNo = houseNo
      |
      |  def setHouseNo(self, houseNo):
      |    self.houseNo = houseNo
      |""".stripMargin
}
