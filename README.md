SPoG
====

A demonstration of Haskell's postgresql-simple.

There's lots of stuff on the web about this but it's hard to get some simple, straightforward, and complete working code.  This ends now.

This projects takes the perspective of working with an existing database (e.g. you already have a well established JVM codebase around it).

In the schema directory are DDLs for the user, database, and schema.  The application has users who can send messages to one or more other users.

This project also presents a small usage scenario for Vagrant.

More info on [postgres-simple](http://hackage.haskell.org/package/postgresql-simple-0.4.2.1/docs/Database-PostgreSQL-Simple.html)

Building
-----------

    $ vagrant up
    $ vagrant ssh
    # in the vagrant box
    $ cd /vagrant
    $ cabal build
    $ ./dist/build/spog/spog

VM
------
A Vagrantfile is provided for convenient access to linux (Ubuntu 14, x64) for windows users (or anyone else who can't get the haskell-pg bindings to work natively).  [Vagrant](https://docs.vagrantup.com/v2/installation/index.html) is awesome.

This is the box I'm running for this.

    $ vagrant box add ubuntu-14.04 http://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-i386-vagrant-disk1.box

See [here for more Vagrant boxes](http://www.vagrantbox.es).

<code>bootstrap.sh</code> will provision this box the first time it runs.

After getting the box provisioned and the DB set up you can do the following to snapshot an image of it

    $ vagrant package
    $ vagrant box add --name spog package.box

Verify that the box was added. You should see 'spog' plus the base box we started with, e.g. 'ubuntu-14.04'.

   $ vagrant box list

You can use the name 'spog' in config.vm.box in the Vagrantfile to start from this image (after running <code>vagrant destroy</code>).

Haskell
-----------

I don't think this project really requires this (whatever is in haskell-platform should suffice) but, what the hell.  The use of VMs lessens the need to have sandboxes, for example.

    $ cabal update
    $ cabal install Cabal-1.20.0.0 cabal-install-1.20.0.0

To get started do the following (in the VM, after logging in)

    $ cd /vagrant # this folder is mapped to wherever you launched the VM which should be this project's root folder.
    $ cabal configure # only needed the first time and after you've modified build.cabal
    $ cabal build
    $ ./dist/build/spog/spog # runs the app

DB
--------
This is how to set up the database in PG.
NB: very important to switch to the spogdb

    $ sudo -u postgres psql
    #\i /vagrant/schema/role.sql
    #\i /vagrant/schema/db.sql
    #\connect spogdb
    #\i /vagrant/schema/schema.sql
