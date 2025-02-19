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
            val charPos: Int = atoz.indexOf(ch) + 1  // Convert 0-based to 1-based index, ChatGPT translation of line from Pascal
            // indexOf returns -1 if char not found
            if (charPos != -1) {
                var index: Int = charPos + shift
                if (index > 26) {
                    index = index - 26
                }
                // extracts char from string
                result = result + atoz.substring(index, index + 1)
                else {
                    result = result + ch
                }
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
            val charPos: Int = atoz.indexOf(ch) + 1  // Convert 0-based to 1-based index, ChatGPT translation of line from Pascal
            // indexOf returns -1 if char not found
            if (charPos != -1) {
                var index: Int = charPos - shift
                if (index < 1) {
                    index = index + 26
                }
                // extracts char from string
                result = result + atoz.substring(index, index + 1)
                else {
                    result = result + ch
                }
            }
            return result
        }
    }


    // Function without return value (procedure)
    def solve(s: String, maxLabel: Int): Unit = {
        var L: Int = 0
        for (L <- maxLabel to 0 by -1){ // by -1 suggested by ChatGPT
            var currentShift = (stringShift + (26 - L)) % 26
            println("Caesar: " + L + ": " + decrypt(s, currentShift))
        }   
    }



    // Main program execution starts here
    def main(args: Array[String]): Unit = {

        // Calling functions
    
        procString = testString.toUpperCase
        procString = procString.trim
        println("Test String: ", testString)
        encrypted = encrypt(procString, stringShift)
        println("Encrypted String: ", encrypted)
        decrypted = decrypt(encrypted, stringShift)
        println("Decrypted String: ", decrypted)
        println()
        println("Trying all shifts: ")
        solve(encrypted, maxShift)
        println()
    }
}
