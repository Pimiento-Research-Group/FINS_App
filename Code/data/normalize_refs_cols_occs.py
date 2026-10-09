
#!/usr/bin/env python3
# normalize_refs_cols_occs.py
# Normalize human-formatted citation stubs in collections/occurrences and create reference_key
# Usage:
# python normalize_refs_cols_occs.py --in collections.csv --out collections_norm.csv
# python normalize_refs_cols_occs.py --in occurrences.csv --out occurrences_norm.csv

import re
import argparse
import pandas as pd
from unidecode import unidecode

PARTICLES = {
    "da","de","del","della","der","des","di","dos","du",
    "la","las","le","les","lo","los","van","von","vom","zu","zur"
}

def norm_space(s: str) -> str:
    return re.sub(r"\s+", " ", s).strip()

def strip_punct_for_key(s: str) -> str:
    s = re.sub(r"[^a-z0-9&.\s-]", " ", s)
    s = s.replace("-", " ")
    return norm_space(s)

def titlecase_surname_token(token: str) -> str:
    parts = token.split()
    if not parts:
        return token
    out = []
    for p in parts:
        pl = p.lower()
        if pl in PARTICLES:
            out.append(pl)
        else:
            out.append(p[:1].upper() + p[1:].lower())
    return " ".join(out)

def canonical_display_single(surname1: str, year: str) -> str:
    return f"{surname1} {year}"

def canonical_display_double(surname1: str, surname2: str, year: str) -> str:
    return f"{surname1} & {surname2} {year}"

def canonical_display_etal(surname1: str, year: str) -> str:
    return f"{surname1} et al. {year}"

def make_display_and_key_from_stub(stub_raw: str):
    warnings = []
    if not isinstance(stub_raw, str):
        return (None, None, ["not_a_string"])
    s = stub_raw.strip()
    s = s.replace(",", " ").replace(";", " ")
    s = re.sub(r"\(", " ", s)
    s = re.sub(r"\)", " ", s)
    s = re.sub(r"\bet\s+al\b\.?", "et al.", s, flags=re.IGNORECASE)
    s = re.sub(r"\b(and|y)\b", "&", s, flags=re.IGNORECASE)
    s = norm_space(s)
    m_year = re.search(r"\b(\d{4})\s*([a-z])?\.?\s*$", s, flags=re.IGNORECASE)
    if not m_year:
        return (None, None, ["missing_year"])
    year_num = m_year.group(1)
    year_suffix = (m_year.group(2) or "").lower()
    year = f"{year_num}{year_suffix}"
    s_no_year = norm_space(s[:m_year.start()])
    if not s_no_year:
        return (None, None, ["missing_authors"])
    auth_seg = s_no_year
    if re.search(r"\bet al\.\b", auth_seg, flags=re.IGNORECASE):
        first = norm_space(re.split(r"\bet al\.\b", auth_seg, flags=re.IGNORECASE)[0])
        surn1_disp = titlecase_surname_token(first.lower())
        display = canonical_display_etal(surn1_disp, year)
        key = f"{unidecode(first).lower()} et al. {year}"
    elif "&" in auth_seg:
        parts = [norm_space(p) for p in auth_seg.split("&") if p]
        if len(parts) != 2:
            warnings.append("more_than_two_authors_interpreted_as_etal")
            first = parts[0]
            surn1_disp = titlecase_surname_token(first.lower())
            display = canonical_display_etal(surn1_disp, year)
            key = f"{unidecode(first).lower()} et al. {year}"
        else:
            first, second = parts
            surn1_disp = titlecase_surname_token(first.lower())
            surn2_disp = titlecase_surname_token(second.lower())
            display = canonical_display_double(surn1_disp, surn2_disp, year)
            key = f"{unidecode(first).lower()} & {unidecode(second).lower()} {year}"
    else:
        surn1_disp = titlecase_surname_token(auth_seg.lower())
        display = canonical_display_single(surn1_disp, year)
        key = f"{unidecode(auth_seg).lower()} {year}"
    key = strip_punct_for_key(key)
    return (display, key, warnings)

def process_df(df: pd.DataFrame, ref_col: str = "reference"):
    logs = []
    disp_out, key_out = [], []
    for i, v in enumerate(df.get(ref_col, pd.Series(index=df.index, dtype="object"))):
        display, key, warns = make_display_and_key_from_stub(v)
        disp_out.append(display)
        key_out.append(key)
        if warns:
            logs.append({"row": int(i), "original": v, "issues": ";".join(warns)})
    df_out = df.copy()
    df_out["reference_display"] = disp_out
    df_out["reference_key"] = key_out
    log_df = pd.DataFrame(logs)
    return df_out, log_df

def main():
    ap = argparse.ArgumentParser(description="Normalize collections/occurrences reference stubs and create reference_key")
    ap.add_argument("--in", dest="inp", required=True, help="Input CSV path (collections.csv or occurrences.csv)")
    ap.add_argument("--out", dest="outp", required=True, help="Output CSV path")
    ap.add_argument("--log", dest="logp", default=None, help="Optional issues log CSV path")
    ap.add_argument("--refcol", dest="refcol", default="reference", help="Column name holding reference stubs")
    args = ap.parse_args()
    df = pd.read_csv(args.inp)
    df_out, log_df = process_df(df, ref_col=args.refcol)
    df_out.to_csv(args.outp, index=False)
    if args.logp:
        (log_df if log_df is not None else pd.DataFrame(columns=["row","original","issues"])).to_csv(args.logp, index=False)
    n = len(df)
    n_missing_key = int(df_out["reference_key"].isna().sum())
    print(f"Processed {n} rows. Missing keys: {n_missing_key}.")

if __name__ == "__main__":
    main()
