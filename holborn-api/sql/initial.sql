-- psql holborn -f sql/initial.sql

drop table "user" cascade;

create table "user"
    ( id serial primary key
    , username varchar(32) unique
    , signup_email varchar(1024) unique
    , password varchar(256)
    , created timestamp without time zone default (now() at time zone 'utc')
    );
