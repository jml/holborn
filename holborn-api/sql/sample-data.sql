-- password is 'test'
insert into "user" (username, signup_email, password) values ('alice', 'alice@exampe.com', '$2y$04$iTvtwfwFymYDEk9EmC4rkeDD5VD21KgdAfC7Fseqh7CyWXaSIhR8u');

insert into "oauth_token" (description, owner_id, token, permissions) values ('desc', 1, 'test-token', 'Permissions (fromList [Web])');
