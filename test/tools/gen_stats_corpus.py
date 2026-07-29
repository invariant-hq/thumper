# /// script
# requires-python = ">=3.10"
# dependencies = ["numpy>=1.24", "scipy>=1.11"]
# ///
"""Generates and verifies the golden statistics corpus (test/golden/stats.tsv).

The corpus pins lib/stats.ml (RFC 0001 section 3) against independent
reference implementations:

- every expected value is computed here in Python, with quantile cutoff
  decisions taken in *exact* arithmetic (native bignums / Fraction), exactly
  mirroring the exact-integer decisions stats.ml makes;
- where scipy/R semantics apply they are cross-checked against scipy:
  `scipy.stats.mannwhitneyu(method='exact')` for rank-test p-values (tie-free
  cases; scipy's exact method is not tie-corrected either) and for the Moses
  cutoff k (one-sided exact p-values at constructed statistics ARE the null
  CDF, so k is re-derived from scipy alone), `numpy.median` over the pairwise
  differences for the Hodges-Lehmann estimate, `scipy.stats.binom` for the
  binomial median-CI cutoffs, and `numpy.percentile` for the Tukey quartiles.

Determinism: sample data is produced by a fixed LCG on the dyadic grid
k/2^16 and every expected value except `geomean` involves only IEEE-exact
operations (+, -, *, /, sqrt, comparisons), so regeneration is byte-identical
across platforms. The `geomean` rows use libm exp/log: byte-identical on one
platform, tested with a relative tolerance on the OCaml side.

Usage:
    uv run test/tools/gen_stats_corpus.py --write   # (re)generate the corpus
    uv run test/tools/gen_stats_corpus.py           # verify: regenerate in
                                                    # memory, byte-compare
                                                    # against the committed
                                                    # file, run cross-checks

Exit codes: 0 verified/written, 1 mismatch or missing file.

File format: tab-separated, '#' comment lines ignored. Float scalars and
comma-separated vectors are C99 hex floats (lossless round-trip). Vectors for
*_sorted functions are stored sorted; `drifted` vectors are collection order.
Rows:
    median      <data> <expected>
    median_ci   <alpha> <data> <lower> <upper>
    outliers    <data> <count>
    severe      <data> <0|1>
    mw_p        <x> <y> <p>
    drifted     <alpha> <data> <0|1>
    shift       <alpha_per_side> <baseline> <candidate> <est> <lower> <upper>
    geomean     <data> <expected>
"""

from __future__ import annotations

import math
import sys
from fractions import Fraction
from pathlib import Path

try:
    import numpy as np
    from scipy import stats as sps

    HAVE_SCIPY = True
except ImportError:  # degraded mode: corpus still exact, cross-checks skipped
    HAVE_SCIPY = False

CORPUS = Path(__file__).resolve().parent.parent / "golden" / "stats.tsv"

# ----------------------------------------------------------------------------
# Deterministic data: a fixed 64-bit LCG, values on the dyadic grid k/2^16.

M64 = (1 << 64) - 1


class Lcg:
    def __init__(self, seed: int):
        self.s = seed & M64

    def next53(self) -> int:
        self.s = (self.s * 6364136223846793005 + 1442695040888963407) & M64
        return self.s >> 11

    def randint(self, lo: int, hi: int) -> int:
        """Uniform-ish integer in [lo, hi] (modulo bias is irrelevant here)."""
        return lo + self.next53() % (hi - lo + 1)


def grid(k: int) -> float:
    return k / 65536.0


def uvec(rng: Lcg, n: int, lo: int, hi: int, step: int = 1) -> list[float]:
    """n values on the grid, uniform over [lo*step, hi*step] grid units."""
    return [grid(rng.randint(lo, hi) * step) for _ in range(n)]


def skewvec(rng: Lcg, n: int, base: int, spread: int) -> list[float]:
    """Right-skewed cluster: base + min of two uniforms + occasional tail."""
    out = []
    for _ in range(n):
        a = rng.randint(0, spread)
        b = rng.randint(0, spread)
        v = base + min(a, b)
        if rng.randint(0, 9) == 0:  # a mild right tail, ~10% of samples
            v += rng.randint(spread, 3 * spread)
        out.append(grid(v))
    return out


# ----------------------------------------------------------------------------
# Reference implementations, mirroring stats.ml semantics exactly.


def median_sorted(v: list[float]) -> float:
    n = len(v)
    return v[n // 2] if n % 2 else (v[n // 2 - 1] + v[n // 2]) / 2.0


def binom_ci_l(n: int, alpha: float) -> int:
    """Largest l with P(Binomial(n,1/2) < l) <= alpha/2 (exact), clamped >= 1.

    Mirrors stats.ml: the comparison treats the float alpha/2 as an exact
    dyadic rational. alpha * 0.5 is exact in binary, so Fraction(alpha) / 2
    is the same rational.
    """
    half = Fraction(alpha) / 2
    total = 1 << n
    s, l = 0, 0
    while l <= n:
        s2 = s + math.comb(n, l)
        if Fraction(s2, total) <= half:
            s, l = s2, l + 1
        else:
            break
    return max(1, l)


def median_ci(v: list[float], alpha: float) -> tuple[float, float]:
    l = binom_ci_l(len(v), alpha)
    return v[l - 1], v[len(v) - l]


def quartile(v: list[float], p: float) -> float:
    """Type-7 (linear interpolation) quantile of sorted v; mirrors stats.ml."""
    h = p * float(len(v) - 1)
    i = int(math.floor(h))
    frac = h - float(i)
    return v[i] if frac == 0.0 else v[i] + frac * (v[i + 1] - v[i])


def outlier_count(v: list[float]) -> int:
    q1, q3 = quartile(v, 0.25), quartile(v, 0.75)
    iqr = q3 - q1
    lo = q1 - (1.5 * iqr)
    hi = q3 + (1.5 * iqr)
    return sum(1 for x in v if x < lo or x > hi)


def severe_inflation(v: list[float]) -> bool:
    """Criterion's outlier-variance measure; severe iff >= 0.5."""
    n = len(v)
    a = float(n)
    s = 0.0
    for x in v:
        s += x
    mean = s / a
    s2 = 0.0
    for x in v:
        d = x - mean
        s2 += d * d
    var = s2 / (a - 1.0)
    sb = math.sqrt(var)
    if sb == 0.0:
        return False
    sb2 = sb * sb
    ua = mean / a
    ug_min = ua / 2.0
    sg = min(ug_min / 4.0, sb / math.sqrt(a))
    sg2 = sg * sg

    def var_out(c: float) -> float:
        ac = a - c
        return ac / a * (sb2 - (ac * sg2))

    def c_max(x: float) -> float:
        k = ua - x
        d = k * k
        ad = a * d
        k1 = sb2 - (a * sg2) + ad
        k0 = -a * ad
        det = (k1 * k1) - (4.0 * sg2 * k0)
        return float(math.floor(-2.0 * k0 / (k1 + math.sqrt(det))))

    vom = min(var_out(1.0), var_out(min(c_max(0.0), c_max(ug_min)))) / sb2
    return vom >= 0.5


def mw_cum(m: int, n: int) -> list[int]:
    """Exact null CDF counts of the Mann-Whitney U statistic (Gaussian
    binomial coefficients, same recurrence as stats.ml)."""
    mn = m * n
    c = [0] * (mn + 1)
    c[0] = 1
    for j in range(1, n + 1):
        for u in range(j, mn + 1):
            c[u] += c[u - j]
        a = m + j
        for u in range(mn, a - 1, -1):
            c[u] -= c[u - a]
    for u in range(1, mn + 1):
        c[u] += c[u - 1]
    assert c[mn] == math.comb(m + n, n), "MW total mismatch"
    return c


def u2_statistic(x: list[float], y: list[float]) -> int:
    u2 = 0
    for xi in x:
        for yj in y:
            if xi > yj:
                u2 += 2
            elif xi == yj:
                u2 += 1
    return u2


def mw_tails(x: list[float], y: list[float]) -> tuple[int, int]:
    """(smaller exact tail count, total count); mirrors stats.ml rounding."""
    m, n = len(x), len(y)
    cum = mw_cum(m, n)
    total = cum[m * n]
    u2 = u2_statistic(x, y)
    lower = cum[u2 // 2]
    ceil_u = (u2 + 1) // 2
    upper = total if ceil_u == 0 else total - cum[ceil_u - 1]
    return min(lower, upper), total


def mw_p(x: list[float], y: list[float]) -> float:
    q, total = mw_tails(x, y)
    return min(1.0, 2.0 * (float(q) / float(total)))


def drifted(v: list[float], alpha: float) -> bool:
    half = len(v) // 2
    q, total = mw_tails(v[:half], v[half:])
    return Fraction(2 * q, total) < Fraction(alpha)


def moses_k(m: int, n: int, alpha_side: float) -> int:
    """Smallest u with null CDF > alpha_side (exact), clamped >= 1."""
    cum = mw_cum(m, n)
    total = cum[m * n]
    a = Fraction(alpha_side)
    c = 0
    while c <= m * n and Fraction(cum[c], total) <= a:
        c += 1
    return max(1, c)


# Independently verified pins for the protocol shapes and alphas (U2):
# derived from brute-force enumeration of the C(m + n, n) rank arrangements
# and cross-checked against scipy's exact CDF. If mw_cum's recurrence is ever
# edited wrongly, these fail before any row is emitted.
assert moses_k(20, 20, 0.005) == 106
assert moses_k(20, 20, 0.05) == 139
assert moses_k(20, 40, 0.005) == 238
assert moses_k(11, 11, 0.005) == 22


def hl_shift(
    b: list[float], c: list[float], alpha_side: float
) -> tuple[float, float, float]:
    diffs = sorted(ci - bj for ci in c for bj in b)
    est = median_sorted(diffs)
    k = moses_k(len(b), len(c), alpha_side)
    mn = len(diffs)
    return est, diffs[k - 1], diffs[mn - k]


def geomean(v: list[float]) -> float:
    s = 0.0
    for x in v:
        s += math.log(x)
    return math.exp(s / len(v))


# ----------------------------------------------------------------------------
# Cross-checks against scipy/numpy (skipped when unavailable).

checks = {"run": 0, "skipped": 0}


def xcheck(name: str, cond: bool, detail: str = "") -> None:
    checks["run"] += 1
    if not cond:
        raise AssertionError(f"cross-check failed: {name} {detail}")


def xcheck_mw_p(x: list[float], y: list[float], p: float) -> None:
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return
    if len(set(x) | set(y)) < len(x) + len(y):
        return  # scipy's exact method and ours only coincide tie-free
    res = sps.mannwhitneyu(x, y, alternative="two-sided", method="exact")
    xcheck("mannwhitneyu", abs(res.pvalue - p) < 1e-9, f"{res.pvalue} vs {p}")


def xcheck_median_ci(v: list[float], alpha: float) -> None:
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return
    n = len(v)
    l = binom_ci_l(n, alpha)
    if Fraction(sum(math.comb(n, i) for i in range(l)), 1 << n) > Fraction(
        alpha
    ) / 2:
        return  # clamped tiny-n case: no coverage claim to check
    xcheck("binom.cdf low", sps.binom.cdf(l - 1, n, 0.5) <= alpha / 2 + 1e-12)
    xcheck("binom.cdf high", sps.binom.cdf(l, n, 0.5) > alpha / 2 - 1e-12)


def xcheck_hl(b: list[float], c: list[float], est: float) -> None:
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return
    diffs = np.array([ci - bj for ci in c for bj in b])
    xcheck("hl np.median", float(np.median(diffs)) == est)


def xcheck_quartiles(v: list[float]) -> None:
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return
    for p, pct in ((0.25, 25), (0.75, 75)):
        ours = quartile(v, p)
        ref = float(np.percentile(np.array(v), pct))
        xcheck("percentile", abs(ours - ref) <= 1e-9 * max(1.0, abs(ref)))


def sample_with_u(m: int, n: int, c: int) -> tuple[list[float], list[float]]:
    """Tie-free samples (x of size m, y of size n) with #{x_i > y_j} = c."""
    q, r = divmod(c, n)
    y = [10.0 * (j + 1) for j in range(n)]
    x = [10.0 * n + 100.0 + i for i in range(q)]  # q x's above every y
    if r:
        x.append(10.0 * r + 5.0)  # one x between y_(r) and y_(r+1)
    x += [-100.0 - i for i in range(m - len(x))]  # the rest below every y
    return x, y


def xcheck_moses_k(m: int, n: int, alpha_side: float, k: int) -> None:
    """Checks the Moses cutoff against scipy's exact null CDF, independently
    of mw_cum's recurrence: mannwhitneyu(alternative='less', method='exact')
    at a constructed tie-free statistic c IS P(U <= c), and k must be
    max(1, smallest c with P(U <= c) > alpha_side). Binary search keeps the
    number of exact-distribution evaluations logarithmic."""
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return

    def cdf(c: int) -> float:
        x, y = sample_with_u(m, n, c)
        res = sps.mannwhitneyu(x, y, alternative="less", method="exact")
        return float(res.pvalue)

    if cdf(0) > alpha_side + 1e-12:
        c = 0  # tiny-m*n clamp: even the extreme falls short
    else:
        lo, hi = 0, m * n  # invariant: cdf(lo) <= alpha < cdf(hi) = 1
        while hi - lo > 1:
            mid = (lo + hi) // 2
            if cdf(mid) <= alpha_side:
                lo = mid
            else:
                hi = mid
        c = hi
    xcheck("moses k", max(1, c) == k, f"m={m} n={n} a={alpha_side}: "
                                      f"scipy {max(1, c)} vs {k}")


def xcheck_drifted(v: list[float], alpha: float, expected: bool) -> None:
    if not HAVE_SCIPY:
        checks["skipped"] += 1
        return
    half = len(v) // 2
    x, y = v[:half], v[half:]
    if len(set(x) | set(y)) < len(x) + len(y):
        return
    p = sps.mannwhitneyu(x, y, alternative="two-sided", method="exact").pvalue
    if abs(p - alpha) > 1e-6:  # away from the decision boundary
        xcheck("drift decision", (p < alpha) == expected, f"p={p} a={alpha}")


# ----------------------------------------------------------------------------
# Corpus assembly.


def hexf(x: float) -> str:
    return float.hex(x)


def hexv(v: list[float]) -> str:
    return ",".join(float.hex(x) for x in v)


def build() -> str:
    rng = Lcg(0x7468756D706572)  # "thumper"
    rows: list[str] = []

    def emit(*fields: str) -> None:
        rows.append("\t".join(fields))

    # --- median ------------------------------------------------------------
    for v in [
        sorted(skewvec(rng, 11, 6_553_600, 65_536)),
        sorted(skewvec(rng, 20, 6_553_600, 65_536)),
        sorted(uvec(rng, 3, 50, 200)),
        sorted(uvec(rng, 20, 90, 110, step=8192)),  # heavy ties
        sorted(uvec(rng, 40, -300, 300)),  # log-space-like negatives
    ]:
        emit("median", hexv(v), hexf(median_sorted(v)))

    # --- median_ci ----------------------------------------------------------
    ci_vec20 = sorted(skewvec(rng, 20, 6_553_600, 131_072))
    for alpha, v in [
        (0.05, ci_vec20),
        (0.01, ci_vec20),
        (0.05, sorted(skewvec(rng, 11, 655_360, 32_768))),
        (0.05, sorted(skewvec(rng, 40, 6_553_600, 65_536))),
        (0.005, sorted(skewvec(rng, 40, 6_553_600, 65_536))),
        (0.1, sorted(uvec(rng, 11, 100, 400))),
        (0.05, sorted(uvec(rng, 3, 100, 400))),  # tiny n: full-range clamp
        (0.05, sorted(uvec(rng, 20, 10, 20, step=16_384))),  # heavy ties
    ]:
        lo, hi = median_ci(v, alpha)
        xcheck_median_ci(v, alpha)
        emit("median_ci", repr(alpha), hexv(v), hexf(lo), hexf(hi))

    # --- outliers -----------------------------------------------------------
    clean20 = sorted(uvec(rng, 20, 6_500_000, 6_600_000))
    spiky20 = sorted(
        uvec(rng, 17, 6_500_000, 6_600_000)
        + [grid(9_000_000), grid(9_500_000), grid(4_000_000)]
    )
    for v in [
        clean20,
        spiky20,
        sorted(uvec(rng, 20, 95, 105, step=32_768)),  # ties
        [grid(6_553_600)] * 10,  # constant
        sorted(uvec(rng, 37, 1_000_000, 1_100_000) + [grid(2_000_000)] * 3),
    ]:
        xcheck_quartiles(v)
        emit("outliers", hexv(v), str(outlier_count(v)))

    # --- severe -------------------------------------------------------------
    stable20 = sorted(uvec(rng, 20, 6_500_000, 6_600_000))
    one_spike20 = sorted(uvec(rng, 19, 6_500_000, 6_600_000) + [grid(60_000_000)])
    two_spikes11 = sorted(
        uvec(rng, 9, 655_360, 720_000) + [grid(7_000_000), grid(8_000_000)]
    )
    for v in [
        stable20,
        one_spike20,
        two_spikes11,
        [grid(123_456)] * 5,  # zero variance
        sorted(uvec(rng, 40, 6_000_000, 9_000_000)),  # wide but not spiky
    ]:
        emit("severe", hexv(v), str(int(severe_inflation(v))))
    assert any(severe_inflation(v) for v in [one_spike20, two_spikes11]), (
        "corpus must contain a severe=1 case"
    )
    assert not severe_inflation(stable20), "corpus must contain a severe=0 case"

    # --- mw_p ---------------------------------------------------------------
    classic_x = [1.0, 2.0, 3.0]
    classic_y = [4.0, 5.0, 6.0]
    mw_cases = [
        (classic_x, classic_y),  # p = 2/20 = 0.1
        (uvec(rng, 10, 0, 10_000_000), uvec(rng, 10, 0, 10_000_000)),
        (uvec(rng, 20, 6_400_000, 6_700_000), uvec(rng, 20, 6_450_000, 6_750_000)),
        (uvec(rng, 20, 6_400_000, 6_700_000), uvec(rng, 40, 6_500_000, 6_800_000)),
        (uvec(rng, 11, 100_000, 200_000), uvec(rng, 11, 250_000, 350_000)),
        (uvec(rng, 5, 0, 1_000_000), uvec(rng, 6, 0, 1_000_000)),
        (uvec(rng, 10, 10, 20, step=8192), uvec(rng, 10, 12, 22, step=8192)),
        (uvec(rng, 2, 0, 100), uvec(rng, 3, 0, 100)),
    ]
    same = uvec(rng, 15, 0, 1_000_000)
    mw_cases.append((same, list(same)))  # identical vectors: p = 1
    for x, y in mw_cases:
        p = mw_p(x, y)
        xcheck_mw_p(x, y, p)
        emit("mw_p", hexv(x), hexv(y), hexf(p))

    # --- drifted ------------------------------------------------------------
    flat20 = uvec(rng, 20, 6_500_000, 6_600_000)
    step20 = uvec(rng, 10, 6_500_000, 6_600_000) + uvec(
        rng, 10, 7_500_000, 7_600_000
    )
    step11 = uvec(rng, 5, 100_000, 110_000) + uvec(rng, 6, 150_000, 160_000)
    mild20 = uvec(rng, 10, 6_500_000, 6_600_000) + uvec(
        rng, 10, 6_520_000, 6_620_000
    )
    trend40 = [grid(6_500_000 + 10_000 * i + rng.randint(0, 20_000)) for i in range(40)]
    tied_flat = uvec(rng, 20, 10, 12, step=16_384)
    for alpha, v in [
        (0.01, flat20),
        (0.01, step20),
        (0.01, step11),
        (0.05, mild20),
        (0.01, trend40),
        (0.05, uvec(rng, 11, 0, 1_000_000)),
        (0.01, tied_flat),
    ]:
        b = drifted(v, alpha)
        xcheck_drifted(v, alpha, b)
        emit("drifted", repr(alpha), hexv(v), str(int(b)))
    assert drifted(step20, 0.01), "corpus must contain a drifted=1 case"
    assert not drifted(flat20, 0.01), "corpus must contain a drifted=0 case"

    # --- shift --------------------------------------------------------------
    # Log-space-like magnitudes: values around log(1e-6 s) ~ -13.8, on the
    # grid so pairwise differences are exact.
    def logvec(n: int, center: int, spread: int) -> list[float]:
        return uvec(rng, n, center - spread, center + spread)

    b20 = logvec(20, -905_000, 3_000)
    c20 = logvec(20, -903_000, 3_000)
    b40 = logvec(40, -905_000, 3_000)
    c40 = logvec(40, -904_000, 3_000)
    b11 = logvec(11, -905_000, 2_000)
    c11 = logvec(11, -901_000, 2_000)
    tied_b = uvec(rng, 20, 90, 110, step=4_096)
    tied_c = uvec(rng, 20, 92, 112, step=4_096)
    shift_cases = [
        (0.005, b20, c20),
        (0.05, b20, c20),
        (0.025, b20, c20),
        (0.005, b40, c20),  # pooled-2n ratchet shape: m = 40 vs n = 20
        (0.005, b20, c40),
        (0.005, b11, c11),
        (0.05, b11, c11),
        (0.005, uvec(rng, 3, 0, 100), uvec(rng, 3, 200, 300)),  # tiny: clamp
        (0.005, tied_b, tied_c),  # heavy ties
        (0.05, tied_b, tied_c),
        (0.005, [grid(500)] * 5, [grid(700)] * 7),  # constant vectors
        (0.005, b20, list(b20)),  # identical: centered on zero
    ]
    seen_k: set[tuple[int, int, float]] = set()
    for alpha_side, b, c in shift_cases:
        est, lo, hi = hl_shift(b, c, alpha_side)
        xcheck_hl(b, c, est)
        key = (len(b), len(c), alpha_side)
        if key not in seen_k:
            seen_k.add(key)
            xcheck_moses_k(len(b), len(c), alpha_side, moses_k(*key))
        assert lo <= est <= hi, "shift interval must bracket the estimate"
        emit(
            "shift",
            repr(alpha_side),
            hexv(b),
            hexv(c),
            hexf(est),
            hexf(lo),
            hexf(hi),
        )

    # --- geomean ------------------------------------------------------------
    ratios = [grid(rng.randint(58_000, 78_000)) for _ in range(8)]  # ~0.9-1.2
    for v in [
        ratios,
        [grid(65_536)],  # singleton: exactly 1.0
        [grid(655_360), grid(65_536), grid(6_553_600)],
        uvec(rng, 12, 1, 1_000_000),
    ]:
        emit("geomean", hexv(v), hexf(geomean(v)))

    header = [
        "# Golden statistics corpus for lib/stats.ml (RFC 0001 section 3).",
        "# Generated by test/tools/gen_stats_corpus.py - DO NOT EDIT.",
        "#   regenerate: uv run test/tools/gen_stats_corpus.py --write",
        "#   verify:     uv run test/tools/gen_stats_corpus.py",
    ]
    return "\n".join(header + rows) + "\n"


def main() -> int:
    write = "--write" in sys.argv[1:]
    content = build()
    if not HAVE_SCIPY:
        print(
            "WARNING: scipy/numpy unavailable; corpus computed from exact "
            "reference implementations only, scipy cross-checks skipped "
            f"({checks['skipped']} skipped)."
        )
    if write:
        CORPUS.parent.mkdir(parents=True, exist_ok=True)
        CORPUS.write_text(content)
        print(f"wrote {CORPUS} ({len(content.splitlines())} lines, "
              f"{checks['run']} cross-checks passed)")
        return 0
    if not CORPUS.exists():
        print(f"error: {CORPUS} missing; run with --write first", file=sys.stderr)
        return 1
    committed = CORPUS.read_text()
    if committed != content:
        print(
            f"error: {CORPUS} is not byte-identical to a fresh regeneration; "
            "stats.tsv or this script changed without the other",
            file=sys.stderr,
        )
        return 1
    print(f"verified: {CORPUS} byte-identical "
          f"({checks['run']} cross-checks passed)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
