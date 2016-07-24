#!/usr/bin/env python


from setuptools import setup, find_packages
import os.path


setup(
    name="holborn_bb",
    version="1.0.0",
    description="Configuration for Holborn's buildbot.",
    author="Jonathan M. Lange",
    author_email="jml@mumak.net",
    install_requires=[],
    zip_safe=True,
    packages=find_packages('.'),
    classifiers = [
        'Intended Audience :: Developers',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
    ],
)
