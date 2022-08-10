package ai.privado.entrypoint

trait CommandProcessor {
  var config: PrivadoInput
  def process(): Either[String, Unit]
}
