KGPC compiler internals
=======================

Selected public-facing headers in ``KGPC/``.  Hawkmoth surfaces any
``/** ... */`` doc comments found in each file; bare declarations without
a doc comment are not listed.  Add a doc comment to a declaration in the
header to make it appear here.

For an index of every C/H file (including those without doc comments),
see :doc:`../source_map`.

Compilation driver
------------------

.. c:autodoc:: KGPC/compilation_context.h
.. c:autodoc:: KGPC/flags.h
.. c:autodoc:: KGPC/unit_paths.h
.. c:autodoc:: KGPC/unit_registry.h
.. c:autodoc:: KGPC/stacktrace.h
.. c:autodoc:: KGPC/string_intern.h
.. c:autodoc:: KGPC/identifier_utils.h

Runtime
-------

.. c:autodoc:: KGPC/runtime_internal.h
.. c:autodoc:: KGPC/runtime_baseunix_internal.h
.. c:autodoc:: KGPC/textrec_layout.h
.. c:autodoc:: KGPC/format_arg.h
