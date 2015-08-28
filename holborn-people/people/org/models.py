"""
The organisational spine of holborn.

We're following github's model here:
https://help.github.com/articles/permission-levels-for-an-organization-repository/

Quick summary:

There are Organisations, Users and Teams.

Each Organisation has a set of teams, and each Team has
one of the following roles:

* Read (pull, fork, send PRs, comment) [on assigned repos]
* Write (all of Read, write, merge, close PRs) [on assigned repos]
* Admin (Write, create, delete repos, transfer repos, change repo settings) [on assigned repos]
* Owner (Admin, create, delete ANY repo, write any repo)
"""
import datetime
from django.db import models

from django.contrib.auth.models import User


class Organisation(models.Model):
    name = models.CharField(max_length=2**10)
    created = models.DateTimeField(default=datetime.datetime.utcnow)


class Team(models.Model):
    ROLES = (
        ('read', 'Read'),
        ('write', 'Write'),
        ('admin', 'Admin'),
        ('owner', 'Owner'),
    )

    role = models.CharField(max_length=32, choices=ROLES)

    # Teams are limited to an org (no team in two orgs)
    organisation = models.ForeignKey(Organisation)
    created = models.DateTimeField(default=datetime.datetime.utcnow)


class Member(models.Model):
    # A person can be a member in many teams. Owner and admin can be
    # considered special in that being an Owner-member includes all
    # permissions of all read/write/admin teams.  We're not treating
    # that case as special though to avoid losing information when
    # users transition into and out of the Owner team.
    user = models.OneToOneField(User)
    team = models.ManyToManyField(Team)
    created = models.DateTimeField(default=datetime.datetime.utcnow)
