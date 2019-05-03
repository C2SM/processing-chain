Documenting python projects with sphinx
=======================================

`Sphinx <http://www.sphinx-doc.org/en/master/>`_ is a tool for documenting code.
It works well for documenting python-code and has been used to document the
`Processing Chain <https://gitlab.empa.ch/abt503/apps/cosmo_processing_chain>`_.

It relies on two ingredients:

python docstrings
-----------------

::

    def add(x, y):
    """Return the sum of x and y"""
    return x + y

Docstrings are string literals occuring right after a function, class, method or
module definition. They are used to document python objects (functions, classes,
modules).

All objects used by others _should_ have a docstring. Also for personal use it
can be very helpful to document non-trivial functions for future reference.
(Writing documentation also helps me personally to better structure my code)

Docstrings can be accessed programmatically:::

    # in python code
    >>> add.__doc__
    "Return the sum of x and y"
    
    # in ipython
    ln[1]: add?
    Signature: add(x, y)
    Docstring: Return the sum of x and y
    File:      <...>
    Type:      function

There are several conventions on what exactly to put into a docstring and how
to format it. `PEP 257 <https://www.python.org/dev/peps/pep-0257/>`_ gives a
good short overview. A nice formatting convention used in the Processing Chain
are `Numpy Style Docstrings <https://sphinxcontrib-napoleon.readthedocs.io/en/latest/example_numpy.html>`_.


reStructuredText
----------------

reStructuredText is a markup syntax. In it's raw form, it is easier to read than
other markup languages (cf. this document) and thus useful for inline code
documentation among other things.

The basics can be found `here <http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html>`_,
while a more comprehensive resource is `this <http://docutils.sourceforge.net/rst.html>`_ .



Using sphinx
------------

Sphinx uses reStructuredText (other markup languages are possible) and the
docstrings of your code to generate useful documentation (plain text, html or 
pdf). Have a look at the
`quickstart guide <http://www.sphinx-doc.org/en/master/usage/quickstart.html>`_.

The essential part is the
`autodoc extension <http://www.sphinx-doc.org/en/master/usage/extensions/autodoc.html>`_,
which inserts the docstrings into the documentation.

For the impatient:
::

    # In the root directory of your project:
    $ sphinx-quickstart
    # Probably you have to adapt the generated conf.py file by adding
    # your sourcecode to sys.path.
    # Then, in the directory where the Makefile was generated
    $ make html text
    # Now you can find the documentation in the build/-directory

Remarks
-------

*   For pure modules (the processing chain is essentially a script with many
    ancillary functions), there exist further possibilities to automate the
    documentation-process (for example automatically instert docstrings of all
    public functions & classes).
*   There exists the possibility to host documentation at https://readthedocs.org/

    Stuff to consider:
    
    -   This is public.
    -   The code likely also has to be in a public repo.
    -   Some effort has to be put into enabling them to build the documentation (They need all the required packages <amrs>).
*   There exists the possibility to render latex-expressions, altough I haven't
    tested that.
*   There are numpy-extensions for sphinx (rendering numpy-style docstrings).
    The docstings look fine with the standard sphinx-theme, but it may be
    worth checking out.
*   At the moment it is not possible to output pdf-documentation on ddm (some
    latex-dependencies are missing). It should not be too much of a hassle to
    enable that if needed.
*   Sphinx also works for other programming languages.
    