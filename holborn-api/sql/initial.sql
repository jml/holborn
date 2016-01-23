-- psql holborn -f sql/initial.sql

drop table "user" cascade;
drop table "org" cascade;
drop table "team" cascade;
drop table "team_member" cascade;
drop table "pull_request" cascade;
drop table "user_repo" cascade;
drop table "org_repo" cascade;
drop table "pull_request" cascade;


create table "user"
    ( id serial primary key
    , username varchar(32) unique
    , signup_email varchar(1024) unique
    , password varchar(256)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


create table "org"
    ( id serial primary key
    , orgname varchar(32) unique
    , created_by_id int references "user" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


create table "team"
    ( id serial primary key
    , org_id int references "org" (id)
    , team_name varchar(128) unique
    , created_by_id int references "user" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


create table "team_member"
    ( id serial primary key
    , org_id int references "org" (id)
    , team_id int references "team" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


-- User and org repositories are different tables because they have
-- different permissions etc attached to them.
create table "user_repo"
    ( id serial primary key
    , name varchar(128)
    , user_id int references "user" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


create table "org_repo"
    ( id serial primary key
    , name varchar(128)
    , org_id int references "org" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


-- pull requests are from a named ref to a named ref. We include the
-- the full path to the other repo as a first-class object. We can
-- make this fast with function indexes.
create table "pull_request"
    ( id serial primary key
    , name varchar(128)
    , comment text
    , initated_by_id int references "user" (id)
    , from_ref varchar(256) -- e.g. teh/holborn to jml/holborn
    , to_ref varchar(256)
    , created timestamp without time zone default (now() at time zone 'utc')
    );
