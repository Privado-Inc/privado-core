package ai.privado.languageEngine.go.audit.TestData

object AuditTestClassData {
  val user: String = """
      |package entity
      |
      |type User struct {
      |	firstName string
      |}
      |
      |func (u User) getFirstName() string {
      |	return u.firstName
      |}
      |
      |func (u *User) setFirstName(fName string) {
      |	u.firstName = fName
      |}
      |""".stripMargin

  val account: String = """
      |package entity
      |
      |type Account struct {
      |	accountNo string
      |}
      |
      |func (a *Account) setAccountNo(accNo string) {
      |	a.accountNo = accNo;
      |}
      |""".stripMargin

  val address: String = """
        |package entity
        |
        |type Address struct {
        |	houseNo string
        |}
        |
        |func (a Address) getHouseNo() string {
        |	return a.houseNo;
        |}
        |""".stripMargin
}
