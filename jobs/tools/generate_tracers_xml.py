import xml.etree.ElementTree as ET
import xml.dom.minidom
import numpy as np

def generate_tracers_xml(data, nens=-1, restart=False):
    """
    Generate an XML representation for chemtracers.

    Args:
        data (dict):
            A dictionary containing details for chemtracers. Example structure:
            {
                "TRCO2_A": {
                    "oem_cat": "A-CO2, ...",
                    "oem_vp": "GNFR_A, ...",
                    "oem_tp": "GNFR_A-CO2, ..."
                },
                "TRCO2_BG": {
                    "init_name": "CO2"
                },
                "CO2_RA": {},
                "CO2_GPP": {},
                "TRCO2_A-XXX": {"start": 0, "count": 10, "bg": "TRCO2_BG", "ra": "CO2_RA", "gpp": "CO2_GPP"}
            }

    Returns:
        str: The prettyfied XML string.
    """
    tracers = ET.Element("tracers")

    # Iterate over all items in data
    for item_id, item_data in data.items():
        print()
        if any(key == "oem_cat" for key in item_data):
            # Make an OEM tracer
            tracer = ET.SubElement(tracers, "chemtracer", id=item_id)
            ET.SubElement(tracer, "transport", type="char").text = "stdaero" if not item_id.startswith("EM_") else "off"
            ET.SubElement(tracer, "c_solve", type="char").text = "passive"
            ET.SubElement(tracer, "init_mode", type="int").text = "0"
            ET.SubElement(tracer, "unit", type="char").text = "none"
            ET.SubElement(tracer, "oem_tscale", type="int").text = "2"
            ET.SubElement(tracer, "oem_type", type="char").text = "emis"
            for key, value in item_data.items():
                if key.startswith("oem_"):
                    ET.SubElement(tracer, key, type="char").text = value
            if restart and not item_id.startswith("EM_"):
                ET.SubElement(tracer, "oem_restart", type="char").text = "file"
        if item_id.endswith("BG"):
            # Make a background tracer
            tracer_bg = ET.SubElement(tracers, "chemtracer", id=item_id)
            ET.SubElement(tracer_bg, "transport", type="char").text = "stdaero"
            ET.SubElement(tracer_bg, "c_solve", type="char").text = "passive"
            ET.SubElement(tracer_bg, "init_mode", type="int").text = "1"
            ET.SubElement(tracer_bg, "unit", type="char").text = "none"
            ET.SubElement(tracer_bg, "init_name", type="char").text = item_data["init_name"]
            ET.SubElement(tracer_bg, "oem_type", type="char").text = "bg"
            if restart:
                ET.SubElement(tracer_bg, "oem_restart", type="char").text = "file"
            ET.SubElement(tracer_bg, "latbc", type="char").text = "file"
        if any(key == "oem_ftype" for key in item_data):
            # Make a VPRM tracer
            tracer_ra = ET.SubElement(tracers, "chemtracer", id=item_id)
            ET.SubElement(tracer_ra, "transport", type="char").text = "stdaero" if not item_id.startswith("EM_") else "off"
            ET.SubElement(tracer_ra, "c_solve", type="char").text = "passive"
            ET.SubElement(tracer_ra, "init_mode", type="int").text = "0"
            ET.SubElement(tracer_ra, "unit", type="char").text = "none"
            ET.SubElement(tracer_ra, "oem_type", type="char").text = "vprm"
            ET.SubElement(tracer_ra, "oem_ftype", type="char").text = item_data["oem_ftype"]
            if restart and not item_id.startswith("EM_"):
                ET.SubElement(tracer_ra, "oem_restart", type="char").text = "file"
        if item_id.endswith("XXX"):
            # Make a set of ensemble tracers
            for i in np.arange(nens) + 1:
                tracer_xxx = ET.SubElement(tracers, "chemtracer", id=f"TRCO2_A-{i:03}")
                ET.SubElement(tracer_xxx, "transport", type="char").text = "stdaero"
                ET.SubElement(tracer_xxx, "oem_type", type="char").text = "ens"
                ET.SubElement(tracer_xxx, "c_solve", type="char").text = "passive"
                ET.SubElement(tracer_xxx, "init_mode", type="int").text = "0"
                if "bg" in item_data:
                    ET.SubElement(tracer_xxx, "oem_bg_ens", type="char").text = item_data["bg"]
                if "ra" in item_data and "gpp" in item_data:
                    ET.SubElement(tracer_xxx, "oem_vprm_bg_ens", type="char").text = f"{item_data['ra']}, {item_data['gpp']}"
                if restart:
                    ET.SubElement(tracer_xxx, "oem_restart", type="char").text = "file"
                ET.SubElement(tracer_xxx, "unit", type="char").text = "none"

    # Convert to string
    xml_declaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE tracers SYSTEM \"tracers.dtd\">\n"
    xml_string = ET.tostring(tracers, encoding="unicode")
    return xml.dom.minidom.parseString(xml_declaration + xml_string).toprettyxml()
