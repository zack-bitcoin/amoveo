function encrypt(txt, password) {
    system("echo -n \"" txt "\" | openssl enc -aes-256-cbc -pbkdf2 -k \"" password "\" > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return($0)
}
function decrypt(txt, password) {
    system("echo -n \"" txt "\" | openssl enc -d -aes-256-cbc -pbkdf2 -k \""password "\" > " FILE)
    getline < FILE
    close(FILE)
    system("rm " FILE)
    return($0)
}
function decrypt_file(File1, File2, password){
    system("openssl enc -d -aes-256-cbc -pbkdf2 -k \"" password "\" -in " File1 " -out " File2)
}
function encrypt_file(File1, File2, password){
    system("openssl enc -aes-256-cbc -pbkdf2 -k \"" password "\" -in " File1 " -out " File2)
}
BEGIN {
    FILE = "aes_temp"
    message = "hello"
    password = "password"
    e = encrypt(message,password)
    message2 = decrypt(e, password)
    #print(e)
    print(message2)

    encrypt_file("aes.awk", "aes.awk.encrypted", "password")
    decrypt_file("aes.awk.encrypted", "aes2.awk", "password")
    system("rm aes.awk.encrypted")
    system("diff aes.awk aes2.awk")
    system("rm aes2.awk")
}
