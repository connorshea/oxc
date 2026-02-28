/**
 * Find the oxlint version where each lint rule was first introduced.
 *
 * Scans git history to determine which oxlint release first included each lint
 * rule, based on when its source file was added to the repository.
 *
 * Requirements:
 *   Full git history — run `git fetch --tags` if needed.
 *
 * Usage:
 *   node tasks/lint_rules/find_rule_versions.mjs [--json] [--output=<file>]
 */

import { execFile } from "node:child_process";
import { writeFile, readdir, stat } from "node:fs/promises";
import { join, relative, dirname } from "node:path";
import { parseArgs } from "node:util";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

const REPO_ROOT = join(fileURLToPath(import.meta.url), "../../..");
const RULES_DIR = join(REPO_ROOT, "crates/oxc_linter/src/rules");

/** Run a git command and return trimmed stdout. */
async function git(...args) {
  const { stdout } = await execFileAsync("git", args, { cwd: REPO_ROOT });
  return stdout.trim();
}

/**
 * Discover all lint rule source files.
 *
 * Rules exist in one of two forms:
 *   crates/oxc_linter/src/rules/<plugin>/<rule>.rs
 *   crates/oxc_linter/src/rules/<plugin>/<rule>/mod.rs
 *
 * Sub-directories of rule directories (fixers/, tests/, etc.) are excluded.
 *
 * @returns {Promise<Array<{plugin: string, rule: string, path: string, key: string}>>}
 */
async function getRuleFiles() {
  const rules = [];

  const pluginDirs = await readdir(RULES_DIR);

  for (const plugin of pluginDirs.sort()) {
    const pluginPath = join(RULES_DIR, plugin);
    if (!(await stat(pluginPath)).isDirectory()) continue;

    const entries = await readdir(pluginPath);

    for (const entry of entries.sort()) {
      const entryPath = join(pluginPath, entry);
      const entryStat = await stat(entryPath);

      if (entryStat.isFile() && entry.endsWith(".rs") && entry !== "mod.rs") {
        // Plain rule file: <plugin>/<rule>.rs
        const rule = entry.slice(0, -3).replaceAll("_", "-");
        rules.push({
          plugin,
          rule,
          path: relative(REPO_ROOT, entryPath),
          key: `${plugin}/${rule}`,
        });
      } else if (entryStat.isDirectory()) {
        // Directory-based rule: <plugin>/<rule>/mod.rs
        const modFile = join(entryPath, "mod.rs");
        try {
          await stat(modFile);
          const rule = entry.replaceAll("_", "-");
          rules.push({
            plugin,
            rule,
            path: relative(REPO_ROOT, modFile),
            key: `${plugin}/${rule}`,
          });
        } catch {
          // No mod.rs — not a rule directory
        }
      }
    }
  }

  return rules;
}

/**
 * Return the oldest commit hash that first added this file, following renames.
 *
 * @param {string} filePath - repo-relative path
 * @returns {Promise<string|null>}
 */
async function getIntroductionCommit(filePath) {
  const output = await git("log", "--diff-filter=A", "--follow", "--format=%H", "--", filePath);
  const commits = output.split("\n").filter(Boolean);
  // git log is newest-first; the last entry is the original add
  return commits.at(-1) ?? null;
}

/**
 * Return the earliest oxlint release tag that contains the given commit.
 *
 * Handles both historical tag formats:
 *   oxlint_v*  — used up to and including oxlint v1.x
 *   apps_v*    — introduced in later releases
 *
 * Both sets are fetched, merged, sorted by version, and the earliest is returned.
 *
 * @param {string} commit
 * @returns {Promise<string|null>}
 */
async function getFirstOxlintVersion(commit) {
  const [oxlintOut, appsOut] = await Promise.all([
    git("tag", "--contains", commit, "--list", "oxlint_v*", "--sort=version:refname"),
    git("tag", "--contains", commit, "--list", "apps_v*", "--sort=version:refname"),
  ]);
  const tags = [...oxlintOut.split("\n").filter(Boolean), ...appsOut.split("\n").filter(Boolean)];
  if (tags.length === 0) return null;
  tags.sort((a, b) => compareVersionKeys(versionSortKey(a), versionSortKey(b)));
  return tags[0];
}

/**
 * Parse an oxlint version tag into a comparable tuple.
 *
 * @param {string|null} version
 * @returns {[number, number, number]}
 */
function versionSortKey(version) {
  if (!version) return [Infinity, Infinity, Infinity];
  const m = version.match(/(?:oxlint_v|apps_v)(\d+)\.(\d+)\.(\d+)/);
  if (m) return [Number(m[1]), Number(m[2]), Number(m[3])];
  return [Infinity, Infinity, Infinity];
}

/** @param {[number,number,number]} a @param {[number,number,number]} b */
function compareVersionKeys(a, b) {
  return a[0] - b[0] || a[1] - b[1] || a[2] - b[2];
}

async function checkGitState() {
  const [oxlintTags, appsTags] = await Promise.all([
    git("tag", "--list", "oxlint_v*"),
    git("tag", "--list", "apps_v*"),
  ]);
  const count =
    oxlintTags.split("\n").filter(Boolean).length + appsTags.split("\n").filter(Boolean).length;
  if (count === 0) {
    console.error(
      "ERROR: No oxlint_v* or apps_v* tags found locally.\n" +
      "Run: git fetch --unshallow --tags\n" +
      "to fetch the full history and all release tags.",
    );
    process.exit(1);
  }
  console.error(`Found ${count} oxlint release tags.`);
}

async function main() {
  const { values } = parseArgs({
    options: {
      json: { type: "boolean" },
      output: { type: "string" },
    },
  });

  await checkGitState();

  console.error("Discovering rule files...");
  const rules = await getRuleFiles();
  console.error(`Found ${rules.length} rules. Querying git history...`);

  // Process all rules concurrently, reporting progress every 50.
  let completed = 0;

  const results = await Promise.all(
    rules.map(async (rule) => {
      const commit = await getIntroductionCommit(rule.path);
      const introducedIn = commit ? await getFirstOxlintVersion(commit) : null;

      completed += 1;
      if (completed % 50 === 0 || completed === rules.length) {
        console.error(`  Processed ${completed}/${rules.length} rules...`);
      }

      return { ...rule, introductionCommit: commit, introducedIn };
    }),
  );

  // Sort by (version, plugin, rule)
  results.sort((a, b) => {
    const vCmp = compareVersionKeys(versionSortKey(a.introducedIn), versionSortKey(b.introducedIn));
    if (vCmp !== 0) return vCmp;
    if (a.plugin !== b.plugin) return a.plugin.localeCompare(b.plugin);
    return a.rule.localeCompare(b.rule);
  });

  let outputText;

  if (values.json) {
    outputText = JSON.stringify(results, null, 2);
  } else {
    const lines = [];
    let currentVersion = null;
    for (const r of results) {
      const v = r.introducedIn ?? "unknown (pre-release or untagged)";
      if (v !== currentVersion) {
        currentVersion = v;
        lines.push(`\n## ${v}`);
      }
      lines.push(`  ${r.key}`);
    }
    outputText = lines.join("\n").trimStart();
  }

  if (values.output) {
    await writeFile(values.output, outputText + "\n", "utf8");
    console.error(`Results written to ${values.output}`);
  } else {
    console.log(outputText);
  }
}

await main();
