# -*- coding: utf-8 -*-
# Generated by Django 1.10.1 on 2016-09-16 16:07
from __future__ import unicode_literals

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='Org',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=256)),
                ('description', models.TextField()),
                ('created_at', models.DateTimeField()),
            ],
        ),
        migrations.CreateModel(
            name='OrgRepo',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=256)),
                ('description', models.TextField()),
                ('hosted_on', models.CharField(max_length=256)),
                ('created_at', models.DateTimeField()),
                ('org', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='core.Org')),
            ],
        ),
        migrations.CreateModel(
            name='SSHKey',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('submitted_key', models.TextField()),
                ('type', models.CharField(choices=[('RSA', 'RSA'), ('DSA', 'DSA')], max_length=3)),
                ('key', models.TextField()),
                ('comment', models.TextField(null=True)),
                ('verified', models.BooleanField()),
                ('readonly', models.BooleanField()),
                ('created_at', models.DateTimeField()),
                ('owner', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
        ),
        migrations.CreateModel(
            name='UserRepo',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=256)),
                ('description', models.TextField()),
                ('hosted_on', models.CharField(max_length=256)),
                ('created_at', models.DateTimeField()),
                ('user', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
        ),
        migrations.RunSQL("alter table core_sshkey alter created_at  set default (now() at time zone 'utc')"),
        migrations.RunSQL("alter table core_org alter created_at  set default (now() at time zone 'utc')"),
        migrations.RunSQL("alter table core_orgrepo alter created_at  set default (now() at time zone 'utc')"),
        migrations.RunSQL("alter table core_userrepo alter created_at  set default (now() at time zone 'utc')"),
    ]