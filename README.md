Outdated!

LockNut-Encryption:
=====================

Overview  

Features  

Encryption Technique  


Overview:
=====================

LockNut is a text file encryption program. It allows you to quickly and easily encrypt
pre-existing files with a password and decrypt those files without any risk of your file
being damaged or tampered with. Compared to other file security programs that protect your
files with a password, only allowing access once the correct password has been given;
LockNut uniquely encrypts your file with the password given. This means that even if your
file is in the hands of another party, the only way it can be made read-able is with the
password you used to encrypt it.


Features:
=====================

LockNut has two main modes Encrypt/Decrypt and Glance. We'll talk about Encrypt/Decrypt
first.

When you open LockNut and click the Encrypt/Decrypt button, you'll be prompted to select a
file. If you choose a .txt file, LockNut will recognize that you want to encrypt the file.
Next, if you've specified a password, it will grab the password you've entered in the
Password box; otherwise it'll use the default password to encrypt your file. You'll be
notified on the status line when the encryption process is complete. The original .txt
file that you choose no longer exists; in it's place is your encrypted file, of the same
name, under the file extension .locknut. The .locknut file should not be editted. You may
open it in Notepad to see what your encrypted file looks like, but if you change anything,
you won't get your original file back when you decrypt it!

Now that you have an encrypted .locknut file, you can use the Encrypt/Decrypt button again
to convert it back to a read-able .txt file. LockNut will automatically detect that the
file is encrypted because of the file extension. It will grab the password you've entered
in the Password box and attempt to convert the .locknut file. Before changing the file or
creating a decrypted .txt version, LockNut will compare your password to the password used
to encrypt the file. The encryption password is stored securely in the .locknut file
itself, and doesn't exist anywhere else. If an incorrect password is given, LockNut will
stop the decryption process, notify you that an incorrect password was given, and leave
the .locknut file unchanged. Given the correct password, the .locknut file will be removed
and the decrypted plain-text version of your file will be added.

The Glance button allows you to look at the decrypted contents of an encrypted .locknut
file without removing the .locknut file or creating a decrypted .txt file. LockNut goes
through all the same verification processes for Glance as it does for Encrypt/Decrypt. If
an incorrect password is given, the file won't be opened and you'll be notified. Any
changes made to the Glance version won't be saved. If you want to edit your file, you must
decrypt it first, make your changes, and then re-encrypt them.


Encryption Technique:
=====================

LockNut uses a complex Vigenere Cipher style encryption technique to encrypt files. A
typical Vigenere Cipher uses a key to shift each "place'th" letter by "place" amount.
Given A = 1, B = 2, etc. A Vigenere Cipher with key "ABC", and message "Hello there" would
do the following:

Unecrypted: 	
Hello there  
ABCABCABCAB  

	H	e	l	l	o	_	...
	+1	+2	+3	+1	+2	+3	...

Encrypted:
IfOmd UjGSu	

Though it may appear strong, Vigenere Ciphers are still vulnerable to letter frequency
analysis. This comes the fact that the shift amounts can only be from +1 to +26, for each
letter in the alphabet; and that keys are typically fairly short.

LockNut accounts for both of these pitfalls. To start, LockNut shifts both up and down the
full 16 bit ASCII table, allowing for 64,000+ potential shift amounts. Additionally, the
input password is uniquely expanded to 100 characters; significantly stronger than even
the most robust personal passwords. Additionally, unlike the Vigenere Cipher, which always
shifts the 1st character in the message by the 1st character in the key, and so on,
LockNut uses the input password again to determine a unique starting position anywhere in
the expanded key.

This second feature is used to foil brute-force attacks run through a third party program,
and not LockNut itself. Given an encryption that only uses 9 bits of the ASCII table (eg.
approximately 512 characters, a typical range of output characters for English text run
through LockNut). Taking into account the typical range of characters in English text and
passwords, and the encryption key expansion, the possible range of characters in encrypted
text is approximately 195. Given that the key is 100 characters long, there are 195^100
possible encryption keys.

Base Key		0-100  	
Typical Password	32-126   
Starting positions	100   

Full Range		32-226 = 194

100(194^100) = 1.008001e+231

