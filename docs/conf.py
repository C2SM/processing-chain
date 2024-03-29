# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
import os
import sys

sys.path.insert(0, os.path.abspath('..'))  # Root directory
sys.path.insert(0, os.path.abspath('../jobs'))  # Jobs subfolder
sys.path.insert(0, os.path.abspath('../jobs/tools'))  # Tools subfolder

# -- Project information ------------------------------------------------

project = 'Processing Chain'
copyright = '2018-2024, C2SM'
author = 'Processing Chain Administrators'
version = 'v3.1'
release = 'v3.1'

# -- General configuration ------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc', 'sphinx.ext.autosectionlabel', 'sphinx.ext.todo',
    'sphinx_rtd_theme', 'sphinx_copybutton', 'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig', 'sphinx.ext.viewcode', 'sphinx.ext.napoleon'
]

# autodoc options
autodoc_member_order = 'bysource'
toc_object_entries_show_parents = 'all'

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This patterns also effect to html_static_path and html_extra_path
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# Copybutton prompt exceptions
copybutton_prompt_text = r"\$ "
copybutton_prompt_is_regexp = True

# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
html_logo = '_static/processing-chain-logo-small.png'
html_favicon = '_static/processing-chain-favicon.ico'
html_theme_options = {
    'logo_only': True,
    'display_version': True,
    'prev_next_buttons_location': 'bottom',
    'style_external_links': False,
    'vcs_pageview_mode': '',
    'style_nav_header_background': '#5d6d7e',
    # Toc options
    'collapse_navigation': False,
    'sticky_navigation': True,
    'navigation_depth': 2,
    'includehidden': True,
    'titles_only': False
}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True


# Include css file to control disable horizontal table scrollbar
def setup(app):
    app.add_css_file('custom.css')
