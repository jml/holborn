from __future__ import unicode_literals
import datetime

from django.db import models
from django.contrib.auth.models import User



class SSHKey(models.Model):
    submitted_key = models.TextField()
    type = models.CharField(choices=[('RSA', 'RSA'), ('DSA', 'DSA')], max_length=3)
    key = models.TextField()
    comment = models.TextField(null=True)
    owner = models.ForeignKey(User)
    verified = models.BooleanField()
    readonly = models.BooleanField()
    created_at = models.DateTimeField()


class Org(models.Model):
    name = models.CharField(max_length=256)
    description = models.TextField()
    created_at = models.DateTimeField()


class UserRepo(models.Model):
    name = models.CharField(max_length=256)
    description = models.TextField()
    user = models.ForeignKey(User)
    hosted_on = models.CharField(max_length=256) # pointer to the backend
    created_at = models.DateTimeField()

    class Meta:
        unique_together = (("user", "name"),)

class OrgRepo(models.Model):
    name = models.CharField(max_length=256)
    description = models.TextField()
    org = models.ForeignKey(Org)
    hosted_on = models.CharField(max_length=256) # pointer to the backend
    created_at = models.DateTimeField()

    class Meta:
        unique_together = (("org", "name"),)
