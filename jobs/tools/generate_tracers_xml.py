import xml.etree.ElementTree as ET
import xml.dom.minidom

# Types of the ICON-ART tracer elements that are not character strings (see
# tracers.dtd). Elements not listed here get their type from the Python type
# of the configured value.
_ELEMENT_TYPES = {
    'mol_weight': 'real',
    'lifetime': 'real',
    'init_mode': 'int',
    'oem_tscale': 'int',
    'iconv': 'int',
    'iturb': 'int',
    'output_emis': 'int',
}


def _element_type(key, value):
    if key in _ELEMENT_TYPES:
        return _ELEMENT_TYPES[key]
    if isinstance(value, bool):
        return 'char'
    if isinstance(value, int):
        return 'int'
    if isinstance(value, float):
        return 'real'
    return 'char'


def _default_elements(tracer_id, tracer_data):
    """Default elements of a tracer, by tracer kind:
    OEM emissions (has ``oem_cat``), VPRM (has ``oem_ftype``), background
    (name ends in ``BG``), or a plain passive tracer otherwise."""
    diagnostic = tracer_id.startswith("EM_")
    if "oem_cat" in tracer_data or "oem_ftype" in tracer_data:
        elements = {"transport": "off" if diagnostic else "stdaero"}
        if diagnostic:
            elements.update(iconv=0, iturb=0, output_emis=1)
        elements.update(c_solve="passive", init_mode=0, unit="none")
        if "oem_cat" in tracer_data:
            elements.update(oem_tscale=2, oem_type="emis")
        else:
            elements.update(oem_type="vprm")
    elif tracer_id.endswith("BG"):
        elements = dict(transport="stdaero",
                        c_solve="passive",
                        init_mode=1,
                        unit="none",
                        oem_type="bg",
                        latbc="file")
    else:
        elements = dict(transport="stdaero",
                        c_solve="passive",
                        init_mode=0,
                        unit="none")
    return elements


def _add_tracer(parent, tracer_id, elements, attributes):
    tracer = ET.SubElement(parent,
                           "chemtracer",
                           id=tracer_id,
                           **{
                               k: str(v)
                               for k, v in attributes.items()
                           })
    for key, value in elements.items():
        ET.SubElement(tracer, key, type=_element_type(key,
                                                      value)).text = str(value)


def generate_tracers_xml(data):
    """
    Generate the ICON-ART tracer XML (``cart_chemtracer_xml``).

    Each tracer gets a set of default elements depending on its kind:

    - OEM emission tracer: has an ``oem_cat`` key
    - VPRM tracer: has an ``oem_ftype`` key
    - background tracer: name ends in ``BG``
    - any other tracer: a plain passive tracer

    Tracers whose name starts with ``EM_`` are diagnostic (not transported).
    Every other key of a tracer becomes an XML element, overriding the
    default of the same name; an ``attributes`` key sets attributes of the
    ``chemtracer`` element itself. For example, a CH4 tracer with OH
    chemistry and OEM emissions::

        TRCH4:
            attributes: {full: "FALSE", chemtr: "TRUE"}
            tag001: chemtr
            mol_weight: 1.604E-2
            lifetime: 25920000
            transport: stdchem
            unit: mol mol-1
            c_solve: OH
            products: TRCO
            init_mode: 1
            init_name: CH4_BG
            oem_tscale: 1
            oem_cat: agri,CH4_J,other
            oem_vp: GNFR_A,GNFR_A,GNFR_A
            oem_tp: GNFR_A,GNFR_A,GNFR_A
            latbc: file

    Parameters
    ----------
    data : dict
        Tracer name -> dict of tracer settings (the ``tracers`` section of
        config.yaml).

    Returns
    -------
    str
        The pretty-printed XML string.
    """
    tracers = ET.Element("tracers")

    for tracer_id, tracer_data in data.items():
        tracer_data = dict(tracer_data or {})
        attributes = tracer_data.pop("attributes", {})
        elements = _default_elements(tracer_id, tracer_data)
        elements.update(tracer_data)
        _add_tracer(tracers, tracer_id, elements, attributes)

    xml_declaration = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                       "<!DOCTYPE tracers SYSTEM \"tracers.dtd\">\n")
    xml_string = ET.tostring(tracers, encoding="unicode")
    return xml.dom.minidom.parseString(xml_declaration +
                                       xml_string).toprettyxml()
