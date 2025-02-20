object CaesarCipher {

    // Global variables (accessible throughout the program)
    var testString: String = "HAL"
    var procString: String = ""
    var atoz: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var stringShift: Int = 2
    var maxShift: Int = 26
    var encrypted: String = ""
    var decrypted: String = ""

    // Function with return value
    def encrypt(s: String, shift: Int): String = {
        var result: String = ""
        var i: Int = 0

        for (i <- 0 until s.length){
            var ch: String = s.substring(i, i + 1)
            ch = ch.toUpperCase
            val charPos: Int = atoz.indexOf(ch)  
            // indexOf returns -1 if char not found
            if (charPos != -1) {
                // Compute shifted index using mod for proper wrap-around
                var index: Int = (charPos + shift) % 26
                // extracts char from string
                result = result + atoz.substring(index, index + 1)
            } else {
                  result = result + ch
            }
      
        }
        return result
    }

    // Function with return value
    def decrypt(s: String, shift: Int): String = {
        var result: String = ""
        var i: Int = 0

        for (i <- 0 until s.length){
            var ch: String = s.substring(i, i + 1)
            // Using 0-based indexing directly 
            val charPos: Int = atoz.indexOf(ch)
            // indexOf returns -1 if char not found
            if (charPos != -1) {
                // Compute shifted index with wrap-around (adding 26 to avoid negative values)
                var index: Int = (charPos - shift + 26) % 26
                // extracts char from string
                result = result + atoz.substring(index, index + 1)
            } else {
                    result = result + ch
            }
        }
        return result
    
    }

    // Function without return value (procedure)
    def solve(s: String, maxLabel: Int): Unit = {
        var L: Int = 0
        for (L <- maxLabel to 0 by -1){ // by -1 suggested by ChatGPT
            println("Caesar " + L + ": " + encrypt(s, L))
        }   
    }

    // Main program execution starts here
    def main(args: Array[String]): Unit = {

        // Calling functions
    
        procString = testString.toUpperCase
        procString = procString.trim
        println("Test String: " + testString)
        encrypted = encrypt(procString, stringShift)
        println("Encrypted String: " + encrypted)
        decrypted = decrypt(encrypted, stringShift)
        println("Decrypted String: " + decrypted)
        println()
        println("Trying all shifts: ")
        // Uses procString so that encryption produces the expected output
        solve(procString, maxShift)
        println()
    }
}
