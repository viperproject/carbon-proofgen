package viper.carbon.proofgen.util


object StringBuilderExtension {

  implicit class StringBuilderExtension(sb: StringBuilder) {
    def newLine: StringBuilder = sb.append(System.lineSeparator())

    def appendInner(s: String): StringBuilder = {
      sb.append("\"").append(s).append("\"")
    }
  }

}
