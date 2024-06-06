package ai.privado.languageEngine.python.audit.TestData

object AuditTestClassData {
  val user: String = """
      |class User:
      |  def __init__(self, fName):
      |    self.firstName = fName
      |
      |  def getFirstName(self):
      |    return self.firstName
      |  
      |  def setFirstName(self, fName):
      |    self.firstName = fName
      |""".stripMargin

  val account: String = """
      |class Account:
      |  def __init__(self, accNo):
      |    self.accountNo = accNo
      |
      |  def setAccountNo(self, accNo):
      |    self.accountNo = accNo
      |""".stripMargin

  val address: String = """
      |class Address:
      |  def __init__(self, hNo):
      |    self.houseNo = hNo
      |
      |  def setHouseNo(self, hNo):
      |    self.houseNo = hNo
      |""".stripMargin
}
