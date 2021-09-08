#!/bin/bash

username="$(git config user.name)"
echo ${username}

# Hello world!
echo "=== Generating settings.ini file for the Processing Chain ==="
echo
echo "Hello ${username}!"
echo "If you want, you can provide your email address."
echo "This is useful to get notified when the chain fails a job."
echo "You can leave it empty if you don't need this feature."
echo

read -p 'Email address: ' usermail
echo

echo "Your email address has been set to" ${usermail}
echo 

#define the template.
cat << EOF > settings.ini
[User]
Name = ${username}
Mail = ${usermail}
EOF

echo "The settings file has been created. You can modify it for your own needs."
echo "Output of settings.ini:"
echo

cat settings.ini

