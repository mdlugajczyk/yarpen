from setuptools import setup

setup(name='yarpen',
      version='0.1',
      description='Educational Scheme compiler',
      url='none',
      author='Marcin Dlugajczyk',
      author_email='dlugajczykmarcin@gmail.com',
      license='BSD',
      packages=['yarpen'],
      scripts=['bin/yarpen'],
      test_suite='nose.collector',
      test_require=['nose'],
      zip_safe=False)
