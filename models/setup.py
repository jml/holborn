from setuptools import setup, find_packages

setup(
    name='holborn-models',
    version='1.0',

    author='teh & jml',
    scripts=['manage.py'],
    packages=find_packages(),
    classifiers=[
        "License :: OSI Approved :: GNU Affero General Public License v3",
    ],
)
