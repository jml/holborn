-- password is 'test'
insert into "user" (id, username, email) values (1, 'alice', 'alice@exampe.com');

insert into "user_repo" (id, name, description, user_id, hosted_on) values (100, 'testrepo', 'test repository', 1, '127.0.0.1:8080');
