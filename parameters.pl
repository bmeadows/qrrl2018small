/*
 * Name:        parameters.pl
 * Author:      Ben Meadows
 * Date:        2018-02-19
 * Description: This file establishes the parameters for q-RRL used by the architecture encoded in 'qRRL.pl'.
 */

:- dynamic initial_learning_rate_parameter/1, current_learning_rate_parameter/1, send_terminal_output_to/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% System interface settings %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This option pipes the system's output to a specified file instead of the terminal.
% Comment it out to view output on the terminal.
send_terminal_output_to('term_output.txt').

% The feedback level specifies how much information to print in output.
% Silent mode only prints key results and errors. Quiet mode additionally prints intermediate feedback on Q-RRL. Verbose mode prints all available text.
% Options: silent, quiet, verbose.
feedback(quiet).

% This debug mode, if set, define behaviour that registers information on how many CPU-seconds are spent in different functions.
% Options: true, false.
registers(true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% System parameters %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Provide a fixed seed the system's random number generator (e.g., for debugging purposes) or allow selection of a random seed (e.g., for experimental trials).
% Options: false (to set a random seed) or a positive integer representing the desired seed.
random_seed(false).

% Main parameter to vary.
% Percentage of the (possibly reduced) space of object configurations to explore.
percent_of_object_config_space_to_search(3).

% Specify the type of learning to perform.
% Options: executability_condition_learning, causal_law_learning, positive_affordance_learning, negative_affordance_learning.
learning_type(executability_condition_learning).

% Simulate actuator noise by setting this value to a positive number.
% The percentage chance that an action has a random outcome in simulation.
noiseChancePercent(0).

% When simulating actuator noise, this parameter specifies whether to make changes to fluents that are in the relevant (constrained) system description.
% If set to false, any fluent can be changed by noise.
useNoiseRelevantToAction(true).

% Use 'cached' option for e.g. batch testing, having precalculated a single time, and then skip the precalculation stage for each individual run.
% Options: precalculate, cached.
domain_relevance(cached).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% MDP traversal parameters %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Amount of reduction in variance required to initiate splitting a leaf in the BDT.
% Specify a positive integer. 1 was observed to be a reasonable value for Blocks World.
amountVarianceReductionRequired(1).

% Wait N episodes before initially checking convergence, and then variance below a certain threshold for N episodes signifies convergence.
% 50 was observed to be a reasonable value for Blocks World.
convergence_buffer(50).

% Define an absolute limit on number of episodes. It may be possible for Q-values to oscillate in some contexts without converging within a reasinable period.
% Takes any positive integer.
maximum_episode_limit(200).

% The minimum number of configurations to explore.
% 40 was observed to be a reasonable value for Blocks World.
minimum_number_of_searches(40).

% The proportion of configurations to explore before allowing the BDT to split. Range: 0-1.
% Set to a small number to allow the process to examine a number of examples before splitting any nodes, to improve the quality of those first, crucial, splits.
% 0.1 was observed to be a reasonable value for Blocks World.
exploration_before_splitting(0.1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Q-Learning parameters %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify how to calculate Q-values for records stored below the leaf.
% (Q-values predicted by the leaf are always set to CurrentQValue + (Alpha * (RewardValue + (Gamma * FutureValue) - CurrentQValue))).
% 'simple' uses the reward value. 'driessens' is an experimental method borrowed from RRL-TG and is likely to not work.
% Options: simple, driessens
q_value_function(simple).

% Learning rate (gamma) can be slowly decreased over each episode or set to a small value.
% 0.1 is a typical low value.
initial_learning_rate_parameter(0.1).
vary_learning_rate(false).

% Specify how often the learner should choose 'explore' over 'exploit'.
% 0.1 is a typical value.
explore_parameter(0.1). 

% Specify the positive and negative reward values used in RRL.
% In practise, these values do not matter as long as the positive reward is reasonably larger than the negative.
% Previous experimental trials use 10 and 0.
reward_pos(10).
reward_neg(0.0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Axiom construction parameters %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Number of both normal and negated literals a candidate axiom under construction may possess.
% Suggested: 2 or 3.
pos_or_neg_clause_limit_per_axiom(2).

% Multiplier for when the number of random samples to draw during generalisation is calculated.
% 100 was observed to be reasonable for Blocks World, but the system appears to be insensitive to the precise value.
sample_multiplier(100).

% Specify the number of filters to apply (after constructing a final set of axioms) using an oracle. Data is collected initially, and after each filter.
% Provide a non-negative integer. Previous experimental trials use 10. 
number_of_filters(10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Speculative parameters %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Included for future - do not change.
% The system only supports closed-world relational learning.
closed_world(true).
rrl(true).
