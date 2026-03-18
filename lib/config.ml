(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type fork_policy = [ `Never | `Per_case | `Per_sample ]
type gc_policy = [ `None | `Major | `Compact ]
type env_policy = [ `Ignore | `Warn | `Fail ]
type stability = [ `Quick | `Low_noise | `Deterministic ]

type t = {
  profile : string option;
  metrics : Metric.t list;
  budgets : Budget.t list;
  warmup_time : float;
  warmup_runs : int;
  sample_time : float;
  min_samples : int;
  max_samples : int;
  max_time : float;
  target_rel_ci : float option;
  fork_policy : fork_policy;
  gc_policy : gc_policy;
  env_policy : env_policy;
  fail_on_inconclusive : bool;
  fail_on_missing_baseline : bool;
  geometric_scale : float;
  no_compactions : bool;
}

let default =
  {
    profile = None;
    metrics = [ Metric.cpu_time; Metric.wall_time; Metric.alloc_words ];
    budgets = [];
    warmup_time = 0.5;
    warmup_runs = 3;
    sample_time = 0.01;
    min_samples = 10;
    max_samples = 300;
    max_time = 10.0;
    target_rel_ci = Some 0.02;
    fork_policy = `Never;
    gc_policy = `Major;
    env_policy = `Warn;
    fail_on_inconclusive = false;
    fail_on_missing_baseline = false;
    geometric_scale = 1.05;
    no_compactions = false;
  }

let quick =
  {
    default with
    warmup_time = 0.1;
    warmup_runs = 1;
    min_samples = 5;
    max_samples = 50;
    max_time = 2.0;
    target_rel_ci = Some 0.05;
    gc_policy = `None;
  }

let ci =
  {
    default with
    warmup_time = 1.0;
    warmup_runs = 5;
    min_samples = 30;
    max_samples = 1000;
    max_time = 30.0;
    target_rel_ci = Some 0.01;
    fork_policy = `Per_case;
    gc_policy = `Compact;
    env_policy = `Fail;
    fail_on_inconclusive = true;
    fail_on_missing_baseline = true;
  }

let deterministic =
  {
    ci with
    metrics = [ Metric.alloc_words; Metric.minor_alloc_words; Metric.cpu_time ];
    min_samples = 50;
    max_samples = 2000;
    max_time = 60.0;
    target_rel_ci = Some 0.005;
  }

let stability s t =
  match s with
  | `Quick ->
      {
        quick with
        profile = t.profile;
        metrics = t.metrics;
        budgets = t.budgets;
      }
  | `Low_noise ->
      { ci with profile = t.profile; metrics = t.metrics; budgets = t.budgets }
  | `Deterministic ->
      {
        deterministic with
        profile = t.profile;
        metrics = t.metrics;
        budgets = t.budgets;
      }

let profile p t = { t with profile = Some p }
let metrics m t = { t with metrics = m }
let budgets b t = { t with budgets = b }
let warmup_time v t = { t with warmup_time = v }
let warmup_runs v t = { t with warmup_runs = v }
let sample_time v t = { t with sample_time = v }
let min_samples v t = { t with min_samples = v }
let max_samples v t = { t with max_samples = v }
let max_time v t = { t with max_time = v }
let target_rel_ci v t = { t with target_rel_ci = Some v }
let fork v t = { t with fork_policy = v }
let gc v t = { t with gc_policy = v }
let env_policy v t = { t with env_policy = v }
let fail_on_inconclusive v t = { t with fail_on_inconclusive = v }
let fail_on_missing_baseline v t = { t with fail_on_missing_baseline = v }
let geometric_scale v t = { t with geometric_scale = v }
let no_compactions v t = { t with no_compactions = v }

let build t =
  let fail msg = invalid_arg (Printf.sprintf "Thumper.Config: %s" msg) in
  if t.min_samples < 1 then fail "min_samples must be >= 1";
  if t.max_samples < 1 then fail "max_samples must be >= 1";
  if t.min_samples > t.max_samples then
    fail
      (Printf.sprintf "min_samples (%d) > max_samples (%d)" t.min_samples
         t.max_samples);
  if t.warmup_time < 0.0 then fail "warmup_time must be >= 0.0";
  if t.warmup_runs < 0 then fail "warmup_runs must be >= 0";
  if t.sample_time <= 0.0 then fail "sample_time must be > 0.0";
  if t.max_time <= 0.0 then fail "max_time must be > 0.0";
  if t.metrics = [] then fail "metrics must not be empty";
  (match t.target_rel_ci with
  | Some v when v <= 0.0 || v >= 1.0 ->
      fail (Printf.sprintf "target_rel_ci must be in (0, 1), got %g" v)
  | _ -> ());
  t

let get_profile t = t.profile
let get_metrics t = t.metrics
let get_budgets t = t.budgets
let get_warmup_time t = t.warmup_time
let get_warmup_runs t = t.warmup_runs
let get_sample_time t = t.sample_time
let get_min_samples t = t.min_samples
let get_max_samples t = t.max_samples
let get_max_time t = t.max_time
let get_target_rel_ci t = t.target_rel_ci
let get_gc t = t.gc_policy
let get_fork t = t.fork_policy
let get_env_policy t = t.env_policy
let get_fail_on_inconclusive t = t.fail_on_inconclusive
let get_fail_on_missing_baseline t = t.fail_on_missing_baseline
let get_geometric_scale t = t.geometric_scale
let get_no_compactions t = t.no_compactions
