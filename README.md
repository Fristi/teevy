Teevyapp
--

This project contains both infa, backend and frontend.

In the frontend folder you'll find grunt, bower and npm stuff to get going

To get the backend up and running you'll need:

Vagrant / compiled backend
---

1. Vagrant (get it from: http://www.vagrantup.com/)
2. VirtualBox (get it from: https://www.virtualbox.org/)
3. Vagrant hostsupdater (after installing virtual box open a command line: `vagrant plugin install vagrant-hostsupdater`)
4. After that you can issue `vagrant up`. This will take a while...
5. After enter: `sh refresh.sh` from the root folder
6. This script will compile the backend and make sure it will run
7. The hosts updater plugin has edited your host file `/etc/hosts`. So now if you goto teevy.co you'll get a page which is served by the vagrant machine. The files which are been served are in the frontend/static folder. It might be that you won't see styling.. just run the grunt task and make sure it compiles js/css and you'll be fine.

Import database
---

1. Open local terminal and navigate to the root of the project
2. `vagrant ssh` to open a SSH connection to the virtual machine
3. change directory to `/work/releases/1.0.0`
4. Issue the command `psql -U teevy --host=10.1.2.3 --password --dbname=teevy -f teevy.sql`
5. Now enter the password: `blabla`
6. Now you imported the database of version 1.0.0