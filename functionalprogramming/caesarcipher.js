// ChatGPT assisted throughout


// convert any input to uppercase
const toUpperCase = (str) => str.toUpperCase();

// encrypts a string using a Caesar cipher in a functional manner
const caesarEncrypt = (text, shift) => {
    let textUpper = toUpperCase(text);
    return textUpper.split("").map((char) => {
        if (char >= "A" && char <= "Z") {
            let base = "A".charCodeAt(0);
            let offset = char.charCodeAt(0) - base;
            let newOffset = (offset + shift) % 26;
            return String.fromCharCode(base + newOffset);
        } else {
            return char;
        }
    }).join("");
};

// decrypts a string using a Caesar cipher in a functional manner
const caesarDecrypt = (text, shift) => {
    let textUpper = toUpperCase(text);
    return textUpper.split("").map((char) => {
        if (char >= "A" && char <= "Z") {
            let base = "A".charCodeAt(0);
            let offset = char.charCodeAt(0) - base;
            let newOffset = (offset - shift + 26) % 26; // +26 to handle negative shifts
            return String.fromCharCode(base + newOffset);
        }
        else {
            return char;
        }
    }).join("");
};

// solves (breaks) a Caesar cipher, shifts 26 to 0
const solveCaesar = (text) => {
    return Array.from({ length: 27 }, (_, i) => 26 - i).map((shift) => {
        return [shift, caesarEncrypt(text, shift)];
    });
};


// test function
const test = () => {
    const orig = "TeSt";
    const shift = 8;
    const upperOrig = toUpperCase(orig);
    const encrypted = caesarEncrypt(upperOrig, shift);
    const decrypted = caesarDecrypt(encrypted, shift);
    const solved = solveCaesar(upperOrig);
    return { orig, upperOrig, encrypted, decrypted, solved };
};

// run the test
const result = test();
console.log("Original:", result.orig);
console.log("Uppercase Original:", result.upperOrig);
console.log("Encrypted:", result.encrypted);
console.log("Decrypted:", result.decrypted);
console.log("Solved:", result.solved);