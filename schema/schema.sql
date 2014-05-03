/*
spog db tables and other collateral
todo too much privelege is granted to spoguser
*/

CREATE SEQUENCE user_id_seq;
CREATE TABLE users (
   id int8 not null default nextval('user_id_seq'),
   name varchar(64) not null,
   PRIMARY KEY (id),
   unique(name)
);
ALTER SEQUENCE user_id_seq owned by users.id;
GRANT ALL PRIVILEGES ON TABLE users TO spoguser;
GRANT ALL PRIVILEGES ON SEQUENCE user_id_seq TO spoguser;

CREATE SEQUENCE message_id_seq;
CREATE TABLE messages (
   id int8 not null default nextval('message_id_seq'),
   sender int8 not null,
   content varchar(255) not null,
   PRIMARY KEY (id),
   FOREIGN KEY (sender) REFERENCES users(id)
);
ALTER SEQUENCE message_id_seq owned by messages.id;
GRANT ALL PRIVILEGES ON TABLE messages TO spoguser;
GRANT ALL PRIVILEGES ON SEQUENCE message_id_seq TO spoguser;

CREATE TABLE recipients (
   message_id int8 not null,
   recipient_id int8 not null,
   FOREIGN KEY (message_id) REFERENCES messages(id),
   FOREIGN KEY (recipient_id) REFERENCES users(id)
);
GRANT ALL PRIVILEGES ON TABLE recipients TO spoguser;
