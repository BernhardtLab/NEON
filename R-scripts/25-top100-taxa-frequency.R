# Script 25: Top 100 Zooplankton Taxa by Observation Frequency
# Creates:
#   1. A horizontal bar chart of the 100 most frequently observed taxa
#      across all NEON sites (2014-2026), formatted for PPT slides.
#   2. A subset chart restricted to taxa observed at every site (all 7 sites).

library(ggplot2)
library(dplyr)

# ── Load data ──────────────────────────────────────────────────────────────────
zoo <- read.csv("data-processed/zooplankton_2014_2026.csv")
taxa_ref <- read.csv("data-processed/zooplankton_taxon_reference.csv")

# ── Count observations per taxon ───────────────────────────────────────────────
obs_counts <- zoo %>%
  count(taxonID, name = "n_obs")

top100_taxa <- obs_counts %>%
  arrange(desc(n_obs)) %>%
  slice_head(n = 100) %>%
  left_join(taxa_ref %>% select(taxonID, scientificName), by = "taxonID") %>%
  mutate(
    label = if_else(is.na(scientificName), taxonID, scientificName),
    label = reorder(label, n_obs)
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
p_top100 <- ggplot(top100_taxa, aes(x = label, y = n_obs)) +
  geom_col(fill = "#2a7bb5", width = 0.75) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04)),
    labels = scales::comma
  ) +
  labs(
    title = "Top 100 Zooplankton Taxa by Observation Frequency",
    subtitle = "NEON dataset, 2014–2026",
    x = NULL,
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.y  = element_text(face = "italic", size = 6.5),
    axis.text.x  = element_text(size = 8),
    axis.title.x = element_text(size = 9, margin = margin(t = 6)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    plot.title    = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, colour = "grey40", margin = margin(b = 8)),
    plot.margin   = margin(12, 18, 12, 8)
  )

# ── Save ───────────────────────────────────────────────────────────────────────
ggsave(
  filename = "figures/top100_zooplankton_taxa_frequency.png",
  plot     = p_top100,
  width    = 13.33,
  height   = 7.5,
  dpi      = 200
)

message("Saved: figures/top100_zooplankton_taxa_frequency.png")

# ── Taxa present at ALL sites ──────────────────────────────────────────────────
n_sites <- n_distinct(zoo$siteID)   # 7 sites

ubiquitous_taxa <- zoo %>%
  group_by(taxonID) %>%
  summarise(n_sites_present = n_distinct(siteID), .groups = "drop") %>%
  filter(n_sites_present == n_sites)

all_sites_taxa <- obs_counts %>%
  filter(taxonID %in% ubiquitous_taxa$taxonID) %>%
  arrange(desc(n_obs)) %>%
  left_join(taxa_ref %>% select(taxonID, scientificName), by = "taxonID") %>%
  mutate(
    label = if_else(is.na(scientificName), taxonID, scientificName),
    label = reorder(label, n_obs)
  )

# ── Plot: taxa at all sites ────────────────────────────────────────────────────
p_all_sites <- ggplot(all_sites_taxa, aes(x = label, y = n_obs)) +
  geom_col(fill = "#2a9d8f", width = 0.75) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04)),
    labels = scales::comma
  ) +
  labs(
    title = "Zooplankton Taxa Found at All NEON Sites",
    subtitle = paste0("Taxa observed at all ", n_sites, " sites · NEON dataset, 2014–2026"),
    x = NULL,
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.y  = element_text(face = "italic", size = 7),
    axis.text.x  = element_text(size = 8),
    axis.title.x = element_text(size = 9, margin = margin(t = 6)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    plot.title    = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, colour = "grey40", margin = margin(b = 8)),
    plot.margin   = margin(12, 18, 12, 8)
  )

# ── Save ───────────────────────────────────────────────────────────────────────
ggsave(
  filename = "figures/all_sites_zooplankton_taxa_frequency.png",
  plot     = p_all_sites,
  width    = 13.33,
  height   = 7.5,
  dpi      = 200
)

message("Saved: figures/all_sites_zooplankton_taxa_frequency.png")
