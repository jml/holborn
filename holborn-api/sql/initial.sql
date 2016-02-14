-- psql holborn -f sql/initial.sql

drop table if exists "user" cascade;
drop table if exists "org" cascade;
drop table if exists "team" cascade;
drop table if exists "team_member" cascade;
drop table if exists "pull_request" cascade;
drop table if exists "user_repo" cascade;
drop table if exists "org_repo" cascade;
drop table if exists "public_key" cascade;


create table "user"
    ( id serial primary key
    , username varchar(32) unique not null
    , signup_email varchar(1024) not null
    , password varchar(256) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );
insert into "user" (username, signup_email, password) values ('alice', 'alice@exampe.com', 'password');


create table "org"
    ( id serial primary key
    , orgname varchar(32) unique not null
    , created_by_id int references "user" (id) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


create table "team"
    ( id serial primary key
    , org_id int references "org" (id) not null
    , team_name varchar(128) unique not null
    , created_by_id int references "user" (id) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


create table "team_member"
    ( id serial primary key
    , org_id int references "org" (id) not null
    , team_id int references "team" (id) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


-- User and org repositories are different tables because they have
-- different permissions etc attached to them.
create table "user_repo"
    ( id serial primary key
    , name varchar(128) not null
    , user_id int references "user" (id) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


create table "org_repo"
    ( id serial primary key
    , name varchar(128) not null
    , org_id int references "org" (id) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


-- pull requests are from a named ref to a named ref. We include the
-- the full path to the other repo as a first-class object. We can
-- make this fast with function indexes.
create table "pull_request"
    ( id serial primary key
    , name varchar(128) not null
    , comment text not null
    , initated_by_id int references "user" (id) not null
    , from_ref varchar(256) not null -- e.g. teh/holborn to jml/holborn
    , to_ref varchar(256) not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );


create table "public_key"
    ( id serial primary key
    , name varchar(128)
    , pubkey text not null
    , owner_id int references "user" (id) not null
    , verified boolean not null
    , readonly boolean not null
    , created timestamp without time zone default (now() at time zone 'utc') not null
    );
