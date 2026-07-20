#!/usr/bin/env python3
# normalize_refs_refs.py
# Normalize structured author/year/identifier from references.csv and create reference_display + reference_key
# Usage:
#   python normalize_refs_refs.py --in references.csv --out references_norm.csv --log issues_refs.csv

import re
import argparse
import pandas as pd
from unidecode import unidecode
from typing import Union, Optional

PARTICLES = {
    "da","de","del","della","der","des","di","dos","du",
    "la","las","le","les","lo","los","van","von","vom","zu","zur",
    "dal"
}

GROUP_AUTHOR_PHRASES = {
    "the japanese club for fossil shark tooth research",
}

def _norm_ascii_lower(s: str) -> str:
    return norm_space(unidecode(s or "").lower())

def contains_group_author(author_str: str) -> bool:
    s = _norm_ascii_lower(author_str)
    return any(phrase in s for phrase in GROUP_AUTHOR_PHRASES)

# ----------------- utils -----------------
def norm_space(s: str) -> str:
    return re.sub(r"\s+", " ", (s or "")).strip()

def strip_punct_for_key(s: str) -> str:
    s = re.sub(r"[^a-z0-9&.\s-]", " ", s)
    s = s.replace("-", " ")
    return norm_space(s)

def titlecase_surname_token(token: str) -> str:
    parts = token.split()
    if not parts: return token
    out = []
    for p in parts:
        pl = p.lower()
        out.append(pl if pl in PARTICLES else p[:1].upper() + p[1:].lower())
    return " ".join(out)

def canonical_display_single(surname1: str, year: str) -> str:
    return f"{surname1} {year}"

def canonical_display_double(surname1: str, surname2: str, year: str) -> str:
    return f"{surname1} & {surname2} {year}"

def canonical_display_etal(surname1: str, year: str) -> str:
    return f"{surname1} et al. {year}"

# --------------- author parsing ---------------
def _is_initials_token(tok: str) -> bool:
    """
    Treat as initials if:
      - it's a single letter (e.g., 'J', 'J,' 'J;'), OR
      - it ends with a dot (e.g., 'J.', 'JP.', 'St.', 'A.-B.', 'J.P.')
    We trim trailing commas/semicolons before testing.
    """
    if not tok:
        return False
    t = tok.strip()
    # tolerate trailing commas/semicolons
    t = re.sub(r"[;,]+$", "", t)

    # single letter (with or without dot)
    if re.fullmatch(r"[A-Za-z]\.?", t):
        return True

    # any token ending with a dot = initials/abbrev (e.g., 'St.', 'Ph.', 'Th.')
    if t.endswith("."):
        return True

    return False

def _valid_surnames(surnames):
    # keep only ones that contain letters (drop '.', '-', etc.)
    return [s for s in surnames if re.search(r"[A-Za-zÀ-ÿ]", s)]

def split_authors(author_str: str):
    """
    Split an 'all authors' field into individual author tokens.
    Handles:
      - Top-level joiners: '&', 'and', 'y', ';'
      - Commas that follow an initial with a dot (e.g., 'T., Next')
      - Removes standalone 'et al.' tokens and punctuation-only tokens
    """
    if not isinstance(author_str, str):
        return []
    s = author_str.strip()

    # Normalize joiners to ';'
    s = s.replace("&", ";")
    s = re.sub(r"\band\b", ";", s, flags=re.IGNORECASE)
    s = re.sub(r"(?<=\s)y(?=\s)", ";", s)  # case-sensitive, requires spaces around
    
    parts = [p.strip() for p in s.split(";") if p.strip()]
    out = []
    for p in parts:
        subs = re.split(r"(?<=\.)\s*,\s*", p)  # split only after initials + dot
        for sp in subs:
            sp = sp.strip()
            # ignore pure years
            if re.fullmatch(r"\d{4}\s*[A-Za-z]?", sp):
                continue
            # ignore 'et al.'
            if re.fullmatch(r"et\s+al\.?,?", sp, flags=re.IGNORECASE):
                continue
            # ignore punctuation-only fragments like ".", ",", "&"
            if not re.search(r"[A-Za-zÀ-ÿ]", sp):
                continue
            out.append(sp)
    return out

def rough_author_count(author_str: str) -> int:
    """
    Conservative top-level author counter based ONLY on joiners (&, 'and', 'y', ';').
    Ignores commas entirely and strips any 'et al.' text first.
    """
    if not isinstance(author_str, str): return 0
    s = re.sub(r"\bet\s+al\.?,?", "", author_str.strip(), flags=re.IGNORECASE)
    s = s.replace("&", ";")
    s = re.sub(r"\band\b", ";", s, flags=re.IGNORECASE)
    s = re.sub(r"(?<=\s)y(?=\s)", ";", s)
    parts = [p.strip() for p in s.split(";") if p.strip()]
    return len(parts)

def extract_surname(name_token: str) -> str:
    """
    Extract surname from a single author token.

    Assumptions (your data):
      - Given names are initials only (e.g., 'J', 'J.', 'J P', 'J.P.', possibly with commas).
    Strategy:
      1) If there's a comma: take the text before the first comma, then strip
         leading/trailing initials inside that left segment.
      2) Otherwise: drop initials at both ends; whatever remains (all words) is the surname.
      3) Return the remaining words joined with spaces (keeps compounds & particles).
    """
    if not name_token:
        return ""
    t = norm_space(name_token)

    def drop_trailing_initials(ws):
        k = len(ws) - 1
        while k >= 0 and _is_initials_token(ws[k]):
            k -= 1
        return ws[:k+1]

    def drop_leading_initials(ws):
        j = 0
        while j < len(ws) and _is_initials_token(ws[j]):
            j += 1
        return ws[j:]

    # Case 1: comma present -> process left side, strip initials there too
    if "," in t:
        left = norm_space(t.split(",", 1)[0])
        words = left.split()
        words = drop_trailing_initials(words)
        words = drop_leading_initials(words)
        return norm_space(" ".join(words)) if words else left

    # Case 2: no comma -> strip initials at ends and keep all remaining words
    words = t.split()
    words = drop_trailing_initials(words)
    words = drop_leading_initials(words)

    # If nothing left (extreme edge), fall back to original token
    return norm_space(" ".join(words)) if words else t

# --------------- main builder ---------------
def build_display_and_key_from_struct(author: str,
                                      year: Union[str, int],
                                      ident: Optional[str]):
    warnings = []

    # year + optional identifier letter (e.g., 1974a)
    y = str(year).strip() if year is not None else ""
    if not re.fullmatch(r"\d{4}", y):
        warnings.append("invalid_year"); return (None, None, warnings)
    ident_letter = ""
    if isinstance(ident, str):
        m = re.match(r"\s*([A-Za-z])\s*\.?\s*$", ident)
        if m: ident_letter = m.group(1).lower()
    year_full = f"{y}{ident_letter}"

    # tokenization + surnames
    tokens = split_authors(author)
    if not tokens:
        warnings.append("missing_authors"); return (None, None, warnings)
    surnames = [extract_surname(t) for t in tokens]
    surnames = [s for s in surnames if s]
    surnames = _valid_surnames(surnames)
    if not surnames:
        warnings.append("missing_surnames"); return (None, None, warnings)

    # presence of explicit 'et al.' means 3+ by intent
    force_etal = bool(re.search(r"\bet\s+al\b", author or "", flags=re.IGNORECASE))
    top_n = rough_author_count(author)

    # ---- choose pattern (conservative) ----
    force_etal = bool(re.search(r"\bet\s+al\b", author or "", flags=re.IGNORECASE))
    if contains_group_author(author):
        force_etal = True

    top_n = rough_author_count(author)

    # If we only have ONE valid surname, it must be single-author
    if len(surnames) == 1 and not force_etal:
        first = surnames[0]
        disp1 = titlecase_surname_token(first.lower())
        display = canonical_display_single(disp1, year_full)
        key = f"{unidecode(first).lower()} {year_full}"
    elif force_etal or top_n >= 3 or len(surnames) >= 3:
        first = surnames[0]
        disp1 = titlecase_surname_token(first.lower())
        display = canonical_display_etal(disp1, year_full)
        key = f"{unidecode(first).lower()} et al. {year_full}"
    else:
        # two-author path (truncate to first two if more leaked through)
        first = surnames[0]
        second = surnames[1] if len(surnames) >= 2 else surnames[0]  # defensive
        disp1 = titlecase_surname_token(first.lower())
        disp2 = titlecase_surname_token(second.lower())
        display = canonical_display_double(disp1, disp2, year_full)
        key = f"{unidecode(first).lower()} & {unidecode(second).lower()} {year_full}"

    key = strip_punct_for_key(key)
    return (display, key, warnings)

# --------------- dataframe pipeline ---------------
def process_df(df: pd.DataFrame, author_col="author", year_col="year", ident_col="identifier"):
    logs = []
    disp_out, key_out = [], []
    for i in range(len(df)):
        a = df.at[i, author_col] if author_col in df.columns else None
        y = df.at[i, year_col] if year_col in df.columns else None
        idc = df.at[i, ident_col] if ident_col in df.columns else None
        display, key, warns = build_display_and_key_from_struct(a, y, idc)
        disp_out.append(display); key_out.append(key)
        if warns:
            logs.append({"row": int(i), "original_author": a, "year": y, "identifier": idc, "issues": ";".join(warns)})
    df_out = df.copy()
    df_out["reference_display"] = disp_out
    df_out["reference_key"] = key_out
    log_df = pd.DataFrame(logs)
    return df_out, log_df

# --------------- CLI ---------------
def main():
    ap = argparse.ArgumentParser(description="Normalize References references to match collections/occurrences reference_display and reference_key.")
    ap.add_argument("--in",  dest="inp", required=True, help="Input CSV path (references.csv)")
    ap.add_argument("--out", dest="outp", required=True, help="Output CSV path")
    ap.add_argument("--log", dest="logp", default=None,   help="Optional issues log CSV path")
    ap.add_argument("--authorcol", dest="authorcol", default="author",     help="Author column name")
    ap.add_argument("--yearcol",   dest="yearcol",   default="year",       help="Year column name")
    ap.add_argument("--identcol",  dest="identcol",  default="identifier", help="Identifier (letter) column name")
    args = ap.parse_args()

    df = pd.read_csv(args.inp)
    df_out, log_df = process_df(df, author_col=args.authorcol, year_col=args.yearcol, ident_col=args.identcol)
    df_out.to_csv(args.outp, index=False)
    if args.logp:
        (log_df if log_df is not None else pd.DataFrame(columns=["row","original_author","year","identifier","issues"])
        ).to_csv(args.logp, index=False)
    n = len(df)
    n_missing_key = int(df_out["reference_key"].isna().sum())
    print(f"Processed {n} rows. Missing keys: {n_missing_key}.")

if __name__ == "__main__":
    main()
