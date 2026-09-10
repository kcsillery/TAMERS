import csv
import os
import urllib.parse
from artifact_tool import Workbook, SpreadsheetFile

input_path = "/mnt/data/Forest_Other_species_nativeness_byCountry.csv"
xlsx_path = "/mnt/data/Forest_Other_species_nativeness_byCountry_corrected.xlsx"
csv_path = "/mnt/data/Forest_Other_species_nativeness_byCountry_corrected.csv"

with open(input_path, newline="", encoding="utf-8-sig") as f:
    original_rows = list(csv.reader(f))

headers = original_rows[0]
species_order = [row[0] for row in original_rows[1:]]
countries = headers[1:]

country_codes = {
    "Albania": "AL",
    "Austria": "AT",
    "Belgium": "BE",
    "Bosnia & Herzegovina": "BA",
    "Bulgaria": "BG",
    "Croatia": "HR",
    "Czechia": "CZ",
    "Denmark": "DK",
    "Finland": "FI",
    "France": "FR",
    "Germany": "DE",
    "Greece": "GR",
    "Hungary": "HU",
    "Ireland": "IE",
    "Italy": "IT",
    "Kosovo": "XK",
    "Latvia": "LV",
    "Montenegro": "ME",
    "North Macedonia": "MK",
    "Norway": "NO",
    "Poland": "PL",
    "Portugal": "PT",
    "Romania": "RO",
    "Serbia": "RS",
    "Slovakia": "SK",
    "Slovenia": "SI",
    "Spain": "ES",
    "Sweden": "SE",
    "Switzerland": "CH",
    "Turkey": "TR",
    "Ukraine": "UA",
    "United Kingdom": "UK",
}
ALL = set(country_codes.values())
BALKAN = {"BA", "HR", "XK", "ME", "MK", "RS", "SI"}

def S(*codes):
    return set(codes)

# Native country assignments. Species labels are preserved exactly as supplied.
native = {
    "Abies concolor": S(),
    "Abies grandis": S(),
    "Abies nordmanniana": S("TR"),
    "Acer monspeliensis": S("AL","BG","FR","DE","GR","IT","PT","RO","ES","CH","TR") | BALKAN,
    "Acer opalus": S("AL","FR","DE","GR","HU","IT","ES","CH","TR") | BALKAN,
    "Acer spicatum": S(),
    "Araucaria araucana": S(),
    "Calocedrus decurrens": S(),
    "Carya spp.": S(),
    "Cedrus libani": S("TR"),
    "Celtis tournefortii": S("AL","BG","GR","IT","UA","TR") | BALKAN,
    "Cryptomeria japonica": S(),
    "Cupressus sempervirens": S("GR","TR"),
    "Cupressus sp.": S("GR","TR"),
    "Eucalyptus globulus": S(),
    "Eucalyptus nitens": S(),
    "Eucalyptus spp.": S(),
    "Fraxinus pennsylvanica": S(),
    "Ginkgo biloba": S(),
    "Juniperus virginiana": S(),
    "Larix x eurolepis": S(),
    "Liquidambar spp.": S("GR","TR"),
    "Liriodendron tulipifera": S(),
    "Paulownia spp.": S(),
    "Pinus brutia": S("BG","GR","TR","UA"),
    "Pinus caribaea": S(),
    "Pinus ponderosa": S(),
    "Pinus radiata": S(),
    "Pinus taeda": S(),
    "Prunus spinosa": ALL,
    "Pseudotsuga menziesii": S(),
    "Quercus castaneifolia": S(),
    "Quercus dalechampii": S("IT"),
    "Quercus polycarpa": S("AT","BG","CZ","SK","GR","HU","RO","TR") | BALKAN,
    "Quercus pyrenaica": S("FR","IT","PT","ES"),
    "Quercus rubra": S(),
    "Quercus virgiliana": S("AL","AT","BE","BG","CZ","SK","FR","DE","GR","HU","IT","UA","RO","ES","CH","TR") | BALKAN,
    "Quercus zeen": S("PT","ES"),
    "Robinia pseudoacacia": S(),
    "Sequoiadendron giganteum": S(),
    "Taxodium distichum": S(),
    "Thuja plicata": S(),
    "Tsuga heterophylla": S(),
    "Tsuga spp.": S(),
    "Ziziphus jujuba": S(),
    "Abies bornmuelleriana": S("TR"),
    "Abies cephalonica": S("AL","GR"),
    "Abies cilicica": S("TR"),
    "Abies equi-trojani": S("TR"),
    "Abies nebrodensis": S("IT"),
    "Abies numidica": S(),
    "Abies pinsapo": S("ES"),
    "Acer campestre": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","UA","PL","RO","ES","SE","CH","TR") | BALKAN,
    "Acer pseudoplatanus": S("AL","AT","BG","CZ","SK","FR","DE","GR","HU","IT","UA","PL","PT","RO","ES","CH","TR") | BALKAN,
    "Arbutus unedo": S("AL","BG","FR","GR","IT","PT","ES","TR") | BALKAN,
    "Carpinus spp.": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","LV","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Castanea sativa": S("AL","GR","TR") | BALKAN,
    "Cornus spp.": ALL,
    "Fagus sylvatica": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","LV","NO","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Ilex aquifolium": S("AL","AT","BE","BG","DK","FR","DE","UK","GR","IE","IT","NO","PT","RO","ES","CH") | BALKAN,
    "Juglans regia": S("TR"),
    "Juniperus communis": ALL,
    "Malus sylvestris": ALL,
    "Picea abies": S("AL","AT","BG","CZ","SK","FI","FR","DE","GR","HU","IT","LV","NO","PL","RO","SE","CH","TR","UA") | BALKAN,
    "Picea omorika": S("BA","RS"),
    "Pinus cembra": S("AT","CZ","SK","FR","DE","IT","PL","RO","CH","UA") | BALKAN,
    "Pinus halepensis": S("AL","FR","GR","IT","ES","TR") | BALKAN,
    "Pinus nigra salzmannii": S("FR","ES"),
    "Pinus nigra subsp. laricio": S("FR","IT"),
    "Pinus pinaster": S("FR","IT","PT","ES"),
    "Pinus pinea": S("AL","FR","GR","IT","PT","ES","TR"),
    "Pinus uncinata": S("AT","CZ","SK","FR","DE","IT","ES","CH"),
    "Populus tremula": ALL,
    "Prunus avium": ALL - S("FI","LV"),
    "Quercus cerris": S("AL","AT","BG","CZ","SK","FR","GR","HU","IT","RO","CH","TR") | BALKAN,
    "Quercus petraea": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IE","IT","LV","NO","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Quercus pubescens": S("AL","AT","BE","BG","CZ","SK","FR","DE","GR","HU","IT","UA","RO","ES","CH","TR") | BALKAN,
    "Quercus robur": ALL,
    "Quercus spp.": ALL,
    "Quercus suber": S("FR","IT","PT","ES"),
    "Sorbus torminalis": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","PL","PT","RO","ES","CH","TR","UA") | BALKAN,
    "Tilia cordata": ALL - S("IE","PT"),
    "Tilia platyphyllos": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Tilia tomentosa": S("AL","BG","GR","HU","RO","TR","UA") | BALKAN,
    "Acer platanoides": S("AL","AT","BE","BG","CZ","SK","FI","FR","DE","GR","HU","IT","LV","NO","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Acer tataricum": S("AL","AT","BG","CZ","SK","GR","HU","RO","TR","UA") | BALKAN,
    "Alnus glutinosa": ALL,
    "Alnus spp.": ALL,
    "Betula celtiberica": S("PT","ES","UK"),
    "Betula pubescens": S("AT","BE","CZ","SK","DK","FI","FR","DE","UK","HU","IE","IT","LV","NO","PL","PT","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Carpinus betulus": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","LV","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Corylus avellana": ALL,
    "Corylus colurna": S("AL","BG","GR","RO","TR") | BALKAN,
    "Fraxinus angustifolia": S("AL","AT","BG","CZ","SK","FR","GR","HU","IT","PT","RO","ES","TR","UA") | BALKAN,
    "Fraxinus angustifolia subsp. pannonica": S("AL","AT","BG","CZ","SK","FR","GR","HU","IT","RO","ES","TR","UA") | BALKAN,
    "Fraxinus excelsior": S("AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IE","IT","LV","NO","PL","RO","ES","SE","CH","TR","UA") | BALKAN,
    "Fraxinus ornus": S("AL","AT","BG","CZ","SK","GR","HU","IT","RO","ES","CH","TR") | BALKAN,
    "Pyrus spp.": S("AL","AT","BE","BG","CZ","SK","FR","DE","GR","HU","IT","LV","PL","PT","RO","ES","CH","TR","UA") | BALKAN,
    "Quercus faginea": S("PT","ES"),
    "Quercus frainetto": S("AL","BG","GR","HU","IT","RO","TR") | BALKAN,
    "Quercus ilex": S("AL","CZ","SK","FR","GR","IT","PT","ES","CH","TR") | BALKAN,
    "Quercus lusitanica": S("PT","ES"),
    "Quercus palustris": S(),
    "Salix caprea": ALL - S("PT"),
    "Sambucus spp.": ALL,
    "Sorbus aria": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","PL","PT","RO","ES","CH") | BALKAN,
    "Sorbus aucuparia": ALL,
    "Sorbus domestica": S("AL","AT","BE","BG","CZ","SK","FR","DE","GR","HU","IT","RO","ES","CH","UA") | BALKAN,
    "Sorbus montanus": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","PL","PT","RO","ES","CH") | BALKAN,
    "Ulmus glabra": ALL - S("PT"),
    "Ulmus laevis": S("AT","BE","BA","BG","HR","CZ","FI","FR","DE","HU","XK","LV","ME","MK","PL","RO","RS","SK","SI","ES","CH","TR","UA"),
    "Ulmus minor": S("AL","AT","BE","BG","CZ","SK","DK","FR","DE","UK","GR","HU","IT","LV","PL","PT","RO","ES","SE","CH","TR","UA") | BALKAN,
}

missing = [sp for sp in species_order if sp not in native]
extra = [sp for sp in native if sp not in species_order]
assert not missing, f"Missing mappings: {missing}"
assert not extra, f"Extra mappings: {extra}"
assert len(species_order) == 112
assert all(set(v).issubset(ALL) for v in native.values())

corrected_rows = []
for sp in species_order:
    corrected_rows.append(
        [sp] + [1 if country_codes[country] in native[sp] else 0 for country in countries]
    )

# Write corrected CSV with the same species labels and row order.
with open(csv_path, "w", newline="", encoding="utf-8-sig") as f:
    writer = csv.writer(f)
    writer.writerow(headers)
    writer.writerows(corrected_rows)

# Internal interpretations used only for checking. Original species names remain unchanged.
interpretations = {
    "Acer monspeliensis": ("Acer monspessulanum", "Spelling interpreted against the accepted name.", "High"),
    "Abies bornmuelleriana": ("Abies nordmanniana subsp. equi-trojani", "Checked under its accepted synonym.", "High"),
    "Abies equi-trojani": ("Abies nordmanniana subsp. equi-trojani", "Checked under its accepted synonym.", "High"),
    "Cupressus sp.": ("Cupressus", "Genus-level response: 1 where at least one native Cupressus species occurs.", "Medium"),
    "Carpinus spp.": ("Carpinus", "Genus-level response: 1 where at least one native Carpinus species occurs.", "Medium"),
    "Cornus spp.": ("Cornus", "Genus-level response: 1 where at least one native Cornus species occurs.", "Medium"),
    "Alnus spp.": ("Alnus", "Genus-level response: 1 where at least one native Alnus species occurs.", "Medium"),
    "Pyrus spp.": ("Pyrus", "Genus-level response: 1 where at least one native Pyrus species occurs.", "Medium"),
    "Quercus spp.": ("Quercus", "Genus-level response: 1 where at least one native Quercus species occurs.", "Medium"),
    "Sambucus spp.": ("Sambucus", "Genus-level response: 1 where at least one native Sambucus species occurs.", "Medium"),
    "Liquidambar spp.": ("Liquidambar", "Genus-level response, based on native Liquidambar orientalis.", "Medium"),
    "Pinus nigra salzmannii": ("Pinus nigra subsp. salzmannii", "Infraspecific rank interpreted without changing the supplied label.", "High"),
    "Fraxinus angustifolia subsp. pannonica": ("Fraxinus angustifolia subsp. oxycarpa", "Checked under the accepted synonym.", "High"),
    "Quercus virgiliana": ("Quercus pubescens subsp. pubescens", "Checked under the accepted synonym.", "High"),
    "Quercus zeen": ("Quercus canariensis", "Interpreted as zeen oak. Verify if another taxon was intended.", "Medium"),
    "Sorbus aria": ("Aria edulis", "Checked under the accepted name used by POWO.", "High"),
    "Sorbus domestica": ("Cormus domestica", "Checked under the accepted name used by POWO.", "High"),
    "Sorbus torminalis": ("Aria torminalis", "Checked under the accepted name used by POWO.", "High"),
    "Sorbus montanus": ("Aria edulis", "Ambiguous supplied name, provisionally treated as Sorbus aria.", "Low"),
}
genus_zero = {
    "Carya spp.": "Carya",
    "Eucalyptus spp.": "Eucalyptus",
    "Paulownia spp.": "Paulownia",
    "Tsuga spp.": "Tsuga",
}
for original, match in genus_zero.items():
    interpretations[original] = (
        match,
        f"Genus-level response: no native member of {match} occurs in the listed countries.",
        "Medium",
    )

# Default interpretation and source URL.
interpretation_rows = []
for sp in species_order:
    match, note, confidence = interpretations.get(
        sp, (sp, "Checked as written against native distribution records.", "High")
    )
    source_url = "https://powo.science.kew.org/results?q=" + urllib.parse.quote_plus(match)
    interpretation_rows.append([sp, match, confidence, note, source_url])

# Count changed cells versus the uploaded matrix.
original_values = {
    row[0]: [int(x) for x in row[1:]] for row in original_rows[1:]
}
changed_by_species = []
for row in corrected_rows:
    sp = row[0]
    changes = sum(a != b for a, b in zip(row[1:], original_values[sp]))
    changed_by_species.append([sp, changes])

# Build polished workbook.
wb = Workbook.create()

sheet = wb.worksheets.add("Nativeness")
sheet.get_range(f"A1:AG{len(corrected_rows)+1}").values = [headers] + corrected_rows
sheet.tables.add(f"A1:AG{len(corrected_rows)+1}", True, "NativenessTable")
sheet.freeze_panes.freeze_rows(1)
sheet.freeze_panes.freeze_columns(1)

header_fmt = {
    "fill": "#166534",
    "font": {"bold": True, "color": "#FFFFFF"},
    "horizontal_alignment": "center",
    "vertical_alignment": "center",
    "wrap_text": True,
    "borders": {
        "bottom": {"style": "thin", "color": "#14532D"},
        "right": {"style": "thin", "color": "#D1D5DB"},
    },
}
sheet.get_range("A1:AG1").format = header_fmt
sheet.get_range("A1:AG1").format.row_height = 46
sheet.get_range(f"A2:A{len(corrected_rows)+1}").format = {
    "font": {"italic": True, "color": "#111827"},
    "vertical_alignment": "center",
}
sheet.get_range(f"B2:AG{len(corrected_rows)+1}").format = {
    "horizontal_alignment": "center",
    "vertical_alignment": "center",
    "number_format": "0",
}
sheet.get_range(f"A2:AG{len(corrected_rows)+1}").format.row_height = 20
sheet.get_range(f"A1:A{len(corrected_rows)+1}").format.column_width = 34
sheet.get_range(f"B1:AG{len(corrected_rows)+1}").format.column_width = 12
sheet.get_range(f"B2:AG{len(corrected_rows)+1}").conditional_formats.add_cell_is(
    {"operator": "equalTo", "formula": 1, "format": {"fill": "#DCFCE7", "font": {"color": "#166534", "bold": True}}}
)
sheet.get_range(f"B2:AG{len(corrected_rows)+1}").conditional_formats.add_cell_is(
    {"operator": "equalTo", "formula": 0, "format": {"fill": "#F3F4F6", "font": {"color": "#6B7280"}}}
)

summary = wb.worksheets.add("Country summary")
summary.get_range("A1:B1").values = [["Country", "Number of listed taxa coded native"]]
summary.get_range("A2:A33").values = [[c] for c in countries]
summary.get_range("B2:B33").formulas = [
    [f"=SUM(Nativeness!{chr(66+i) if i < 25 else ''}2:{chr(66+i) if i < 25 else ''}113)"]
    for i in range(32)
]
# Replace formulas above with robust Excel column letters.
def excel_col(n):
    result = ""
    while n:
        n, rem = divmod(n - 1, 26)
        result = chr(65 + rem) + result
    return result
summary.get_range("B2:B33").formulas = [
    [f"=SUM(Nativeness!{excel_col(i+2)}2:{excel_col(i+2)}113)"] for i in range(32)
]
summary.tables.add("A1:B33", True, "CountrySummaryTable")
summary.freeze_panes.freeze_rows(1)
summary.get_range("A1:B1").format = header_fmt
summary.get_range("A1:A33").format.column_width = 28
summary.get_range("B1:B33").format.column_width = 20
summary.get_range("B2:B33").format = {"horizontal_alignment": "center", "number_format": "0"}

notes = wb.worksheets.add("Methodology")
notes_rows = [
    ["Corrected country-specific nativeness matrix"],
    [""],
    ["Coding rule", "1 = the taxon is accepted as native in the country; 0 = it is not native there."],
    ["Status rule", "Introduced, cultivated, naturalized, or planted occurrences are coded 0."],
    ["Species labels", "Every Species entry is preserved exactly as supplied and remains in the same row order."],
    ["Taxonomic matching", "Accepted names and synonyms were used only behind the scenes to retrieve distributions."],
    ["Genus-level entries", "For spp. or sp. entries, 1 means at least one accepted native member of the genus occurs in the country."],
    ["Regional mapping", "WCVP often reports NW. Balkan Peninsula rather than modern national borders. Those records were expanded to Bosnia & Herzegovina, Croatia, Kosovo, Montenegro, North Macedonia, Serbia, and Slovenia."],
    ["Caution", "The NW. Balkan expansion can overestimate nativeness at individual national borders. Review these cells if exact national floras are required."],
    ["Ambiguous entries", "Quercus zeen and Sorbus montanus were interpreted as documented on the Taxon interpretation sheet."],
    [""],
    ["Primary source", "Plants of the World Online / World Checklist of Vascular Plants, Royal Botanic Gardens, Kew"],
    ["Source URL", "https://powo.science.kew.org/"],
    ["Geographic standard", "World Geographical Scheme for Recording Plant Distributions, WGSRPD Level 3"],
    ["WGSRPD URL", "https://www.tdwg.org/standards/wgsrpd/"],
    ["Dataset access", "https://sftp.kew.org/pub/data-repositories/WCVP/"],
]
notes.get_range(f"A1:B{len(notes_rows)}").values = notes_rows
notes.merge_cells("A1:B1")
notes.get_range("A1").format = {
    "fill": "#14532D",
    "font": {"bold": True, "color": "#FFFFFF", "size": 16},
    "horizontal_alignment": "left",
    "vertical_alignment": "center",
}
notes.get_range("A1:B1").format.row_height = 30
notes.get_range(f"A3:A{len(notes_rows)}").format = {"font": {"bold": True, "color": "#166534"}}
notes.get_range(f"A1:B{len(notes_rows)}").format.wrap_text = True
notes.get_range(f"A1:A{len(notes_rows)}").format.column_width = 24
notes.get_range(f"B1:B{len(notes_rows)}").format.column_width = 92
notes.get_range(f"A2:B{len(notes_rows)}").format.row_height = 34

interp = wb.worksheets.add("Taxon interpretation")
interp_headers = ["Species as supplied", "Internal match used", "Confidence", "Interpretation note", "POWO search URL"]
interp.get_range(f"A1:E{len(interpretation_rows)+1}").values = [interp_headers] + interpretation_rows
interp.tables.add(f"A1:E{len(interpretation_rows)+1}", True, "InterpretationTable")
interp.freeze_panes.freeze_rows(1)
interp.get_range("A1:E1").format = header_fmt
interp.get_range(f"A1:A{len(interpretation_rows)+1}").format.column_width = 34
interp.get_range(f"B1:B{len(interpretation_rows)+1}").format.column_width = 38
interp.get_range(f"C1:C{len(interpretation_rows)+1}").format.column_width = 12
interp.get_range(f"D1:D{len(interpretation_rows)+1}").format.column_width = 62
interp.get_range(f"E1:E{len(interpretation_rows)+1}").format.column_width = 60
interp.get_range(f"A2:E{len(interpretation_rows)+1}").format.wrap_text = True
interp.get_range(f"A2:E{len(interpretation_rows)+1}").format.row_height = 34

changes = wb.worksheets.add("Change summary")
changes.get_range(f"A1:B{len(changed_by_species)+1}").values = [["Species", "Country cells changed"]] + changed_by_species
changes.tables.add(f"A1:B{len(changed_by_species)+1}", True, "ChangeSummaryTable")
changes.freeze_panes.freeze_rows(1)
changes.get_range("A1:B1").format = header_fmt
changes.get_range(f"A1:A{len(changed_by_species)+1}").format.column_width = 34
changes.get_range(f"B1:B{len(changed_by_species)+1}").format.column_width = 20
changes.get_range(f"B2:B{len(changed_by_species)+1}").format = {"horizontal_alignment": "center", "number_format": "0"}

SpreadsheetFile.export_xlsx(wb).save(xlsx_path)

print(f"Created: {xlsx_path}")
print(f"Created: {csv_path}")
print(f"Rows: {len(corrected_rows)}, country columns: {len(countries)}")
print(f"Species labels preserved exactly: {species_order == [r[0] for r in corrected_rows]}")


# Compact verification of the workbook and exported CSV.
check_matrix = wb.inspect({
    "kind": "table",
    "range": "Nativeness!A1:H12",
    "include": "values,formulas",
    "table_max_rows": 12,
    "table_max_cols": 8,
})
print(check_matrix.ndjson)

check_summary = wb.inspect({
    "kind": "table",
    "range": "Country summary!A1:B10",
    "include": "values,formulas",
    "table_max_rows": 10,
    "table_max_cols": 2,
})
print(check_summary.ndjson)

errors = wb.inspect({
    "kind": "match",
    "search_term": "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
    "options": {"use_regex": True, "max_results": 100},
    "summary": "final formula error scan",
})
print(errors.ndjson)

with open(csv_path, newline="", encoding="utf-8-sig") as f:
    exported = list(csv.reader(f))

assert exported[0] == headers
assert [row[0] for row in exported[1:]] == species_order
assert len(exported) == 113
assert all(len(row) == 33 for row in exported)
assert all(value in {"0", "1"} for row in exported[1:] for value in row[1:])

print("CSV validation passed: 112 unchanged species rows and 3,584 binary country cells.")
