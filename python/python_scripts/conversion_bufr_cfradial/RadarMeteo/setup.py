import os

MAINTAINER = 'Departamento de Investigacion y Desarrollo, SMN'
MAINTAINER_EMAIL = '[lvidal mrugna]@smn.gov.ar'
URL = 'https://gitlab.smn.gov.ar/ID/RadarMeteo'
MAJOR = 0
MINOR = 2
MICRO = 4
VERSION = '%d.%d.%d' % (MAJOR, MINOR, MICRO)

# BEFORE importing distutils, remove MANIFEST. distutils doesn't properly
# update it when the contents of directories change.
if os.path.exists('MANIFEST'):
    os.remove('MANIFEST')

# Pongo la version para que sea mas facil saber que esta instalado o que
# errores estan corregidos


def write_version_py(filename='version.py'):
    cnt = """
# Archivo generado por setup.py para saber la version

version = '%(version)s'
"""
    a = open(filename, 'w')

    try:
        a.write(cnt % {'version': VERSION})
    finally:
        a.close()


def configuration(parent_package='', top_path=None):
    from numpy.distutils.misc_util import Configuration
    # Se puede usar otro nombre con Configuration('nombre')
    # en vez de RadarMeteo
    config = Configuration('radarsmn', parent_package, top_path)
    config.set_options(delegate_options_to_subpackages=True,
                       quiet=True)
    config.add_subpackage('conversores')
    config.add_subpackage('disdrometro')
    config.add_subpackage('QC')

    return config


def setup_package():

    write_version_py()

    from numpy.distutils.core import setup

    setup(
        maintainer=MAINTAINER,
        maintainer_email=MAINTAINER_EMAIL,
        url=URL,
        version=VERSION,
        configuration=configuration
    )


if __name__ == "__main__":
    setup_package()
