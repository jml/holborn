-- psql holborn -f sql/initial.sql

drop table "user" cascade;
drop table "org" cascade;
drop table "team" cascade;
drop table "team_member" cascade;

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


create table "user_repository"
    ( id serial primary key
    , name varchar(128)
    , user_id int references "user" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );


create table "org_repository"
    ( id serial primary key
    , name varchar(128)
    , org_id int references "org" (id)
    , created timestamp without time zone default (now() at time zone 'utc')
    );
