// CHATGPT helped throughout

object functionalcc {

    // Global variables (accessible throughout the program)
    var testString: String = "TeSt" // Use the mixed-case original
    var procString: String = ""
    
    // function with return value that encrypts a string using a Caesar cipher in a functional manner
    def encryptCaesar(input: String, shift: Int): String = {
        // assumes input is already in uppercase
        input.map { char =>
            if (char.isLetter) {
                val base = 'A'
                ((char - base + shift) % 26 + base).toChar
            } else {
                char
            }
        }.mkString
    }
    
    // function with return value that decrypts a string using a Caesar cipher in a functional manner
    def decryptCaesar(input: String, shift: Int): String = {
        input.map { char =>
            if (char.isLetter) {
                val base = 'A'
                ((char - base - shift + 26) % 26 + base).toChar
            } else {
                char
            }
        }.mkString
    }
    
    // function without return value (procedure) that solves (breaks) a Caesar cipher by
    // printing encryptions for each possible shift from 26 down to 0
    def solveCaesar(input: String): Unit = {
        // convert input to uppercase
        val upperInput = input.toUpperCase
        (26 to 0 by -1).foreach { shift =>
            val candidate = encryptCaesar(upperInput, shift)
            println(s"Caesar $shift: $candidate")
        }
    }
        
    // main program
    def main(args: Array[String]): Unit = {
        // convert testString to uppercase
        testString = testString.toUpperCase()
        val shift = 8
        procString = encryptCaesar(testString, shift)
    
       
        println(s"Original: $testString")
        println(s"Encrypted: $procString")
        val decryptedString = decryptCaesar(procString, shift)
        println(s"Decrypted: $decryptedString")
        println()
    
        println("SOLVE for \"TeSt\":")
        solveCaesar("TeSt")
    }
}
