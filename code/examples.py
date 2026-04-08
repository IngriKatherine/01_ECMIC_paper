import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import brentq

# ── shared style ──────────────────────────────────────────────────────────────
plt.rcParams.update({
    "font.family": "sans-serif",
    "axes.spines.top": False,
    "axes.spines.right": False,
})

n = np.linspace(0.05, 1.0, 400)

# ── curve definitions ─────────────────────────────────────────────────────────
def supply(n):
    return 0.4 + 2.2 * n**1.8

def demand(n):
    return 2.5 - 2.2 * n**1.1

def mcl_mono(n):
    return 0.4 + 6.5 * n**1.8

# ── find intersections ────────────────────────────────────────────────────────
n_star  = brentq(lambda x: supply(x) - demand(x), 0.1, 0.9)
w_star  = supply(n_star)

n_m     = brentq(lambda x: mcl_mono(x) - demand(x), 0.1, 0.9)
w_m     = supply(n_m)    # monopsony wage (on supply curve)
mrpl_mc = demand(n_m)    # MRPL = MC level (top of markdown box)


# ══════════════════════════════════════════════════════════════════════════════
# Figure 1 – Labor Market Power (Monopsony)
# ══════════════════════════════════════════════════════════════════════════════
fig1, ax1 = plt.subplots(figsize=(7.5, 6))

ax1.plot(n, supply(n),   color="#8B0000", lw=2.2, label="Worker's Supply")
ax1.plot(n, demand(n),   color="#00008B", lw=2.2,
         label="Firm's Demand\n(Marginal Revenue\nProduct of Labor)")
ax1.plot(n, mcl_mono(n), color="#006400", lw=2.2, label="Marginal Cost of\nLabor")

# ── dashed drop lines ─────────────────────────────────────────────────────────
# Vertical line from x-axis up to the MCL=MRPL intersection
ax1.plot([n_m, n_m], [0, mrpl_mc],       color="gray", lw=1.2, ls="--", zorder=1)
# Horizontal line at MRPL=MC level → y-axis
ax1.plot([0, n_m],   [mrpl_mc, mrpl_mc], color="gray", lw=1.2, ls="--", zorder=1)
# Horizontal line at w_i level → y-axis
ax1.plot([0, n_m],   [w_m, w_m],         color="gray", lw=1.2, ls="--", zorder=1)

# ── wage-markdown shaded rectangle (from y-axis to n_m) ──────────────────────
ax1.fill_between([0, n_m], [w_m, w_m], [mrpl_mc, mrpl_mc],
                 color="silver", alpha=0.55, zorder=2)
ax1.text(n_m / 2, (w_m + mrpl_mc) / 2,
         "W markdown", ha="center", va="center",
         fontstyle="italic", fontsize=10, color="#333")

# ── intersection dots ─────────────────────────────────────────────────────────
ax1.plot(n_m, mrpl_mc, "ko", ms=6, zorder=5)  # MCL = MRPL point
ax1.plot(n_m, w_m,     "ko", ms=6, zorder=5)  # point on supply curve

# ── x-axis: tick + marker line at n_m ────────────────────────────────────────
ax1.set_xticks([n_m])
ax1.set_xticklabels(["$n_i$"], fontsize=12)
ax1.tick_params(axis="x", length=5, width=1.2)

# ── y-axis: ticks at w_i and MRPL=MC ──────────────────────────────────────────
ax1.set_yticks([w_m, mrpl_mc])
ax1.set_yticklabels(["$w_i$", "MRPL=MC"], fontsize=11)
ax1.tick_params(axis="y", length=5, width=1.2, pad=4)

ax1.set_xlabel("Number of\nworkers (n)", fontsize=11)
ax1.set_ylabel("Wage (w)", fontsize=11)
ax1.set_xlim(0, 1.05)
ax1.set_ylim(0, 3.0)

ax1.legend(loc="upper right", fontsize=9, frameon=True)
ax1.set_title("Labor Market Power (Monopsony)", fontsize=13, pad=10)

fig1.tight_layout()
fig1.savefig("C:/Users/k_the/main/01_ECMIC_paper/figures/example_labor_marker_power.png", dpi=150)
print("Figure 1 saved.")


# ══════════════════════════════════════════════════════════════════════════════
# Figure 2 – Perfect Competition
# ══════════════════════════════════════════════════════════════════════════════
fig2, ax2 = plt.subplots(figsize=(7.5, 6))

ax2.plot(n, supply(n), color="#8B0000", lw=2.2, label="Worker's Supply")
ax2.plot(n, demand(n), color="#00008B", lw=2.2,
         label="Firm's Demand\n(Marginal Revenue\nProduct of Labor)")
ax2.axhline(w_star, color="#006400", lw=2.2, label="Marginal Cost of Labor")

# ── dashed drop lines from (n*, w*) to both axes ─────────────────────────────
ax2.plot([n_star, n_star], [0, w_star],       color="gray", lw=1.2, ls="--")
ax2.plot([0,      n_star], [w_star, w_star],  color="gray", lw=1.2, ls="--")

# ── intersection dot ──────────────────────────────────────────────────────────
ax2.plot(n_star, w_star, "ko", ms=6, zorder=5)

# ── axis ticks at intersection values ────────────────────────────────────────
ax2.set_xticks([n_star])
ax2.set_xticklabels(["$n^*$"], fontsize=12)
ax2.tick_params(axis="x", length=5, width=1.2)

ax2.set_yticks([w_star])
ax2.set_yticklabels(["$w^*$"], fontsize=12)
ax2.tick_params(axis="y", length=5, width=1.2, pad=4)

ax2.set_xlabel("Number of\nworkers (n)", fontsize=11)
ax2.set_ylabel("Wage (w)", fontsize=11)
ax2.set_xlim(0, 1.05)
ax2.set_ylim(0, 3.0)

ax2.legend(loc="upper right", fontsize=9, frameon=True)
ax2.set_title("Perfect Competition in the Labor Market", fontsize=13, pad=10)

fig2.tight_layout()
fig2.savefig("C:/Users/k_the/main/01_ECMIC_paper/figures/example_perfect_competition.png", dpi=150)
print("Figure 2 saved.")

plt.show()