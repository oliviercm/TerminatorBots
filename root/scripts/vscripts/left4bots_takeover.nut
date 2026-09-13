//--------------------------------------------------------------------------------------------------
//     GitHub:		https://github.com/smilz0/Left4Bots
//     Workshop:	https://steamcommunity.com/sharedfiles/filedetails/?id=3022416274
//--------------------------------------------------------------------------------------------------

Msg("Including left4bots_takeover...\n");

// Schedules the automatic bot takeover for the given (just died) human player after 'auto_takeover_delay' seconds
::Left4Bots.ScheduleAutoTakeover <- function (player, userid)
{
	local delay = Settings.auto_takeover_delay;
	if (delay < 0)
		delay = 0;

	Logger.Debug("ScheduleAutoTakeover - " + player.GetPlayerName() + " (" + userid + ") in " + delay + " seconds");

	Left4Timers.AddTimer(null, delay, @(params) ::Left4Bots.OnAutoTakeover.bindenv(::Left4Bots)(params), { player = player, userid = userid });
}

// L4D2 survivor character data (index = m_survivorCharacter): model + "who" context (UI/vocalizer)
// The character set for TEAM_SURVIVORS is always the L4D2 one, so a fixed table is sufficient
::Left4Bots.TakeoverChars <- [
	{ model = "models/survivors/survivor_gambler.mdl",    context = "Gambler" },    // 0 Nick
	{ model = "models/survivors/survivor_producer.mdl",   context = "Producer" },   // 1 Rochelle
	{ model = "models/survivors/survivor_coach.mdl",      context = "Coach" },      // 2 Coach
	{ model = "models/survivors/survivor_mechanic.mdl",   context = "Mechanic" },   // 3 Ellis
	{ model = "models/survivors/survivor_namvet.mdl",     context = "NamVet" },     // 4 Bill
	{ model = "models/survivors/survivor_teenangst.mdl",  context = "TeenGirl" },   // 5 Zoey
	{ model = "models/survivors/survivor_biker.mdl",      context = "Biker" },      // 6 Francis
	{ model = "models/survivors/survivor_manager.mdl",    context = "Manager" },    // 7 Louis
]

// Applies the full survivor character state to 'client': visual model, character property and UI/vocalizer context.
// All three are required: the property alone does not change the visual model (and L4D2 vscript has no SetCharacter global)
::Left4Bots.ApplySurvivorCharacter <- function (client, char)
{
	if (!client || !client.IsValid() || char < 0 || char > 7)
		return;

	client.SetModel(TakeoverChars[char].model);
	NetProps.SetPropIntArray(client, "m_survivorCharacter", char, 0);
	client.SetContext("who", TakeoverChars[char].context, -1);
}

// One-shot timer callback scheduled by ScheduleAutoTakeover: makes the player take over a random living survivor bot if the player is still dead
::Left4Bots.OnAutoTakeover <- function (params)
{
	local player = params.player;
	local userid = params.userid;

	if (!player || !player.IsValid())
		return; // The player disconnected or the map changed

	// The death handler removes dead survivors from Survivors and the spawn handler adds them back when they are defibbed/respawned,
	// so this is a reliable "still dead" check (IsDead() alone is not for L4D2 survivors)
	if (userid in Survivors)
		return; // The player was defibbed (or respawned) within the delay window

	if (player.GetZombieType() != 9 || NetProps.GetPropInt(player, "m_iTeamNum") != TEAM_SURVIVORS)
		return; // No longer a survivor

	local bot = GetRandomTakeoverTarget();
	if (!bot)
	{
		Logger.Debug("OnAutoTakeover - no living survivor bot available, " + player.GetPlayerName() + " remains dead");
		return;
	}

	Logger.Info("OnAutoTakeover - " + player.GetPlayerName() + " is taking over bot " + bot.GetPlayerName());

	// The swap phase is full of engine state manipulations that can throw on unexpected data; catch and log
	// (with the exact Squirrel exception) instead of leaving the flow half-applied and silently untracked
	try
	{
		// Characters before the swap: the player takes over the bot's character, the bot (which becomes the corpse) wears the player's
		local playerChar = NetProps.GetPropInt(player, "m_survivorCharacter");
		local botChar = NetProps.GetPropInt(bot, "m_survivorCharacter");

		// A dead L4D2 survivor cannot be revived by swapping entity properties: the engine still tracks the player as dead
		// (survival rules + survivor_death_model) and re-kills them within a second, and a dead client can't pick up weapons.
		// The engine's own defib path handles all of it: it consumes the death model, restores the client and updates its internal state.
		player.ReviveByDefib();

		// Both clients are living survivors now: swap their states.
		// The player gets the bot's body (position, weapons, ammo, health) and the bot inherits the player's original state
		// (death position, character, weapons, ammo)
		SimpleTakeover(player, bot);

		// Apply the full visual character to both clients (model + property + context)
		ApplySurvivorCharacter(player, botChar);
		ApplySurvivorCharacter(bot, playerChar);

		// Finish the takeover: the bot must end up as the dead player. Restore FL_FAKECLIENT (the swap strips it from the flags
		// it assigns) so the death handler recognizes the bot as one and does the full cleanup, then kill it in place -
		// the game spawns its survivor_death_model (wearing the player's character) at the death location
		NetProps.SetPropInt(bot, "m_fFlags", NetProps.GetPropInt(bot, "m_fFlags") | (1 << 8)); // FL_FAKECLIENT

		local botHealth = NetProps.GetPropInt(bot, "m_iHealth");
		bot.TakeDamage(botHealth, 0, null);
		if (NetProps.GetPropInt(bot, "m_iHealth") > 0) // A single hit can be damage-capped
			bot.TakeDamage(NetProps.GetPropInt(bot, "m_iHealth"), 0, Entities.FindByClassname(null, "worldspawn"));
	}
	catch (e)
	{
		Logger.Error("OnAutoTakeover - error: " + e);
	}

	// Re-add to the tracked survivors lists only if the player is actually alive after the attempt
	if (NetProps.GetPropInt(player, "m_iHealth") > 0)
	{
		Survivors[userid] <- player;
		SurvivorFlow[userid] <- { isBot = false, inCheckpoint = IsSurvivorInCheckpoint(player), flow = GetCurrentFlowDistanceForPlayer(player) };
	}

	// Same hygiene pass L4B does in OnGameEvent_bot_player_replace after a client identity swap
	PlayerResetAll(player);

	PrintSurvivorsCount();
}

// Is 'bot' a valid target for the automatic takeover?
::Left4Bots.IsValidTakeoverTarget <- function (bot)
{
	if (!bot || !bot.IsValid())
		return false;

	if (!IsPlayerABot(bot) || !IsHandledBot(bot))
		return false;

	if (NetProps.GetPropInt(bot, "m_iTeamNum") != TEAM_SURVIVORS)
		return false;

	if (NetProps.GetPropInt(bot, "m_iHealth") <= 1)
		return false; // Dead (0) or dying (1) - a raw prop check, IsDead()/IsDying() are unreliable for L4D2 survivors

	if (bot.IsIncapacitated() || bot.IsDominatedBySpecialInfected())
		return false;

	if (bot.GetMoveParent() != null)
		return false; // Carried by a special infected

	local chr = NetProps.GetPropInt(bot, "m_survivorCharacter");
	if (chr < 0 || chr > 7)
		return false;

	return true;
}

// Returns a random living survivor bot (or null if none is available)
::Left4Bots.GetRandomTakeoverTarget <- function ()
{
	local candidates = [];
	foreach (bot in Bots)
	{
		if (IsValidTakeoverTarget(bot))
			candidates.append(bot);
	}

	if (candidates.len() == 0)
		return null;

	return candidates[RandomInt(0, candidates.len() - 1)];
}

// Swaps the state (position, health, character, inventory, ammo, ...) of two survivors: 'main' becomes 'target' and vice versa
// Ported from the "SimpleTakeover" method of VScript Survivor Manager (VSSM) by Shadowysn
// https://steamcommunity.com/sharedfiles/filedetails/?id=3006216183
::Left4Bots.SimpleTakeover <- function (main, target)
{
	if (!main.IsSurvivor() || !target.IsSurvivor())
		return;

	local switchData = [
		NetProps.GetPropInt(target, "m_iTeamNum"), // 0
		NetProps.GetPropInt(main, "m_iTeamNum"),
		target.GetOrigin(), // 2
		main.GetOrigin(),
		target.EyeAngles(), // 4
		main.EyeAngles(),
		NetProps.GetPropInt(target, "m_iObserverMode"), // 6
		NetProps.GetPropInt(main, "m_iObserverMode"),
		NetProps.GetPropInt(target, "m_hObserverTarget"), // 8
		NetProps.GetPropInt(main, "m_hObserverTarget"),
		NetProps.GetPropInt(target, "m_iMaxHealth"), // 10
		NetProps.GetPropInt(main, "m_iMaxHealth"),
		NetProps.GetPropInt(target, "m_iHealth"), // 12
		NetProps.GetPropInt(main, "m_iHealth"),
		NetProps.GetPropInt(target, "m_lifeState"), // 14
		NetProps.GetPropInt(main, "m_lifeState"),
		NetProps.GetPropInt(target, "m_survivorCharacter"), // 16
		NetProps.GetPropInt(main, "m_survivorCharacter"),
		NetProps.GetPropInt(target, "m_zombieClass"), // 18
		NetProps.GetPropInt(main, "m_zombieClass"),
		NetProps.GetPropInt(target, "m_zombieState"), // 20
		NetProps.GetPropInt(main, "m_zombieState"),
		NetProps.GetPropInt(target, "m_isIncapacitated"), // 22
		NetProps.GetPropInt(main, "m_isIncapacitated"),
		NetProps.GetPropInt(target, "m_isHangingFromLedge"), // 24
		NetProps.GetPropInt(main, "m_isHangingFromLedge"),
		NetProps.GetPropVector(target, "m_hangAirPos"), // 26
		NetProps.GetPropVector(main, "m_hangAirPos"),
		NetProps.GetPropVector(target, "m_hangPos"), // 28
		NetProps.GetPropVector(main, "m_hangPos"),
		NetProps.GetPropVector(target, "m_hangStandPos"), // 30
		NetProps.GetPropVector(main, "m_hangStandPos"),
		NetProps.GetPropVector(target, "m_hangNormal"), // 32
		NetProps.GetPropVector(main, "m_hangNormal"),
		NetProps.GetPropInt(target, "m_frustration"), // 34
		NetProps.GetPropInt(main, "m_frustration"),
		NetProps.GetPropInt(target, "m_clientIntensity"), // 36
		NetProps.GetPropInt(main, "m_clientIntensity"),
		NetProps.GetPropInt(target, "m_iPlayerState"), // 38
		NetProps.GetPropInt(main, "m_iPlayerState"),
		NetProps.GetPropInt(target, "pl.deadflag"), // 40
		NetProps.GetPropInt(main, "pl.deadflag"),
		NetProps.GetPropVector(target, "m_vecViewOffset"), // 42
		NetProps.GetPropVector(main, "m_vecViewOffset"),
		NetProps.GetPropInt(target, "m_iBonusProgress"), // 44
		NetProps.GetPropInt(main, "m_iBonusProgress"),
		NetProps.GetPropInt(target, "m_iBonusChallenge"), // 46
		NetProps.GetPropInt(main, "m_iBonusChallenge"),
		NetProps.GetPropEntity(target, "m_hViewEntity"), // 48
		NetProps.GetPropEntity(main, "m_hViewEntity"),
		NetProps.GetPropInt(target, "m_bDucked"), // 50
		NetProps.GetPropInt(main, "m_bDucked"),
		NetProps.GetPropInt(target, "m_bDucking"), // 52
		NetProps.GetPropInt(main, "m_bDucking"),
		NetProps.GetPropInt(target, "m_bInDuckJump"), // 54
		NetProps.GetPropInt(main, "m_bInDuckJump"),
		NetProps.GetPropInt(target, "m_nDuckTimeMsecs"), // 56
		NetProps.GetPropInt(main, "m_nDuckTimeMsecs"),
		NetProps.GetPropInt(target, "m_nDuckJumpTimeMsecs"), // 58
		NetProps.GetPropInt(main, "m_nDuckJumpTimeMsecs"),
		NetProps.GetPropInt(target, "m_nJumpTimeMsecs"), // 60
		NetProps.GetPropInt(main, "m_nJumpTimeMsecs"),
		NetProps.GetPropFloat(target, "m_flFallVelocity"), // 62
		NetProps.GetPropFloat(main, "m_flFallVelocity"),
		NetProps.GetPropEntity(target, "m_hUseEntity"), // 64
		NetProps.GetPropEntity(main, "m_hUseEntity"),
		NetProps.GetPropEntity(target, "m_hRagdoll"), // 66
		NetProps.GetPropEntity(main, "m_hRagdoll"),
		NetProps.GetPropVector(target, "m_lastLadderNormal"), // 68
		NetProps.GetPropVector(main, "m_lastLadderNormal"),
		NetProps.GetPropInt(target, "m_iShovePenalty"), // 70
		NetProps.GetPropInt(main, "m_iShovePenalty"),
		NetProps.GetPropFloat(target, "m_healthBuffer"), // 72
		NetProps.GetPropFloat(main, "m_healthBuffer"),
		NetProps.GetPropFloat(target, "m_healthBufferTime"), // 74
		NetProps.GetPropFloat(main, "m_healthBufferTime"),
		NetProps.GetPropFloat(target, "m_itTimer.m_duration"), // 76
		NetProps.GetPropFloat(main, "m_itTimer.m_duration"),
		NetProps.GetPropFloat(target, "m_itTimer.m_timestamp"), // 78
		NetProps.GetPropFloat(main, "m_itTimer.m_timestamp"),
		NetProps.GetPropInt(target, "m_isFallingFromLedge"), // 80
		NetProps.GetPropInt(main, "m_isFallingFromLedge"),
		NetProps.GetPropInt(target, "m_currentReviveCount"), // 82
		NetProps.GetPropInt(main, "m_currentReviveCount"),
		NetProps.GetPropInt(target, "m_isGoingToDie"), // 84
		NetProps.GetPropInt(main, "m_isGoingToDie"),
		NetProps.GetPropFloat(target, "m_vomitStart"), // 86
		NetProps.GetPropFloat(main, "m_vomitStart"),
		NetProps.GetPropFloat(target, "m_vomitFadeStart"), // 88
		NetProps.GetPropFloat(main, "m_vomitFadeStart"),
		NetProps.GetPropFloat(target, "m_stunTimer.m_duration"), // 90
		NetProps.GetPropFloat(main, "m_stunTimer.m_duration"),
		NetProps.GetPropFloat(target, "m_stunTimer.m_timestamp"), // 92
		NetProps.GetPropFloat(main, "m_stunTimer.m_timestamp"),
		NetProps.GetPropFloat(target, "m_TimeForceExternalView"), // 94
		NetProps.GetPropFloat(main, "m_TimeForceExternalView"),
		target.GetVelocity(), // 96
		main.GetVelocity(),
		NetProps.GetPropInt(target, "m_MoveType"), // 98
		NetProps.GetPropInt(main, "m_MoveType"),
		NetProps.GetPropInt(target, "m_fFlags"), // 100
		NetProps.GetPropInt(main, "m_fFlags"),
		NetProps.GetPropInt(target, "m_iEFlags"), // 102
		NetProps.GetPropInt(main, "m_iEFlags"),
		NetProps.GetPropEntity(target, "m_hGroundEntity"), // 104
		NetProps.GetPropEntity(main, "m_hGroundEntity"),
		NetProps.GetPropVector(target, "m_Collision.m_vecMins"), // 106
		NetProps.GetPropVector(main, "m_Collision.m_vecMins"),
		NetProps.GetPropVector(target, "m_Collision.m_vecMaxs"), // 108
		NetProps.GetPropVector(main, "m_Collision.m_vecMaxs"),
		NetProps.GetPropInt(target, "cslocaldata.m_duckUntilOnGround"), // 110
		NetProps.GetPropInt(main, "cslocaldata.m_duckUntilOnGround"),
	];

	local mainAmmo = {};
	local targetAmmo = {};
	for (local i = 0; i < NetProps.GetPropArraySize(main, "m_iAmmo"); i++)
	{
		local ammo = NetProps.GetPropIntArray(main, "m_iAmmo", i);
		if (ammo == 0)
			continue;
		mainAmmo[i] <- ammo;
		NetProps.SetPropIntArray(main, "m_iAmmo", 0, i); // Clear else we dupe ammo
	}
	for (local i = 0; i < NetProps.GetPropArraySize(target, "m_iAmmo"); i++)
	{
		local ammo = NetProps.GetPropIntArray(target, "m_iAmmo", i);
		if (ammo == 0)
			continue;
		targetAmmo[i] <- ammo;
		NetProps.SetPropIntArray(target, "m_iAmmo", 0, i); // Clear else we dupe ammo
	}
	foreach (key, ammo in mainAmmo)
	{
		NetProps.SetPropIntArray(target, "m_iAmmo", ammo, key);
	}
	foreach (key, ammo in targetAmmo)
	{
		NetProps.SetPropIntArray(main, "m_iAmmo", ammo, key);
	}
	mainAmmo.clear();
	targetAmmo.clear();

	local mainInvTable = {};
	local targetInvTable = {};
	GetInvTable(main, mainInvTable);
	GetInvTable(target, targetInvTable);
	foreach (key, val in mainInvTable)
	{
		local valClass = val.GetClassname();
		if (NetProps.HasProp(val, "m_isDualWielding") && NetProps.GetPropInt(val, "m_isDualWielding") != 0)
		{
			NetProps.SetPropInt(val, "m_isDualWielding", 0);
			local hackWep = SpawnEntityFromTable(valClass, {
				origin = val.GetOrigin().ToKVString(),
				angles = val.GetAngles().ToKVString(),
			});
			DoEntFire("!self", "Use", "", 0, target, hackWep);
		}
		else if (NetProps.HasProp(val, "m_bRedraw") && NetProps.GetPropInt(val, "m_bRedraw") != 0)
		{
			// m_bRedraw is 1 when a grenade has been thrown; killing the entity avoids
			// dupeing it when the other survivor "uses" it. The game still makes it unusable
			// so no infinite grenades regardless
			val.Kill();
			continue;
		}
		main.DropItem(valClass);
		DoEntFire("!self", "Use", "", 0, target, val);
	}
	foreach (key, val in targetInvTable)
	{
		local valClass = val.GetClassname();
		if (NetProps.HasProp(val, "m_isDualWielding") && NetProps.GetPropInt(val, "m_isDualWielding") != 0)
		{
			NetProps.SetPropInt(val, "m_isDualWielding", 0);
			local hackWep = SpawnEntityFromTable(valClass, {
				origin = val.GetOrigin().ToKVString(),
				angles = val.GetAngles().ToKVString(),
			});
			DoEntFire("!self", "Use", "", 0, main, hackWep);
		}
		else if (NetProps.HasProp(val, "m_bRedraw") && NetProps.GetPropInt(val, "m_bRedraw") != 0)
		{
			val.Kill();
			continue;
		}
		target.DropItem(valClass);
		DoEntFire("!self", "Use", "", 0, main, val);
	}

	for (local i = 0; i <= 1; i++)
	{
		local client = (i == 1) ? target : main;

		if (NetProps.GetPropInt(client, "m_isIncapacitated") != 0)
			NetProps.SetPropInt(client, "m_isIncapacitated", 0);

		client.Stagger(Vector());
		NetProps.SetPropFloat(client, "m_staggerTimer.m_duration", 0);
		NetProps.SetPropFloat(client, "m_staggerTimer.m_timestamp", 0);

		NetProps.SetPropInt(client, "m_iTeamNum", switchData[0 + i]);
		client.SetOrigin(switchData[2 + i]);
		client.SnapEyeAngles(switchData[4 + i]);
		NetProps.SetPropInt(client, "m_iObserverMode", switchData[6 + i]);
		NetProps.SetPropInt(client, "m_hObserverTarget", switchData[8 + i]);
		NetProps.SetPropInt(client, "m_iMaxHealth", switchData[10 + i]);
		NetProps.SetPropInt(client, "m_iHealth", switchData[12 + i]);
		NetProps.SetPropInt(client, "m_lifeState", switchData[14 + i]);
		// Character is applied after the swap by ApplySurvivorCharacter (SetModel + m_survivorCharacter + "who" context);
		// L4D2 vscript has no bare SetCharacter global (VSSM defines its own internally)
		NetProps.SetPropInt(client, "m_zombieClass", switchData[18 + i]);
		NetProps.SetPropInt(client, "m_zombieState", switchData[20 + i]);
		NetProps.SetPropInt(client, "m_isIncapacitated", switchData[22 + i]);
		NetProps.SetPropInt(client, "m_isHangingFromLedge", switchData[24 + i]);
		NetProps.SetPropVector(client, "m_hangAirPos", switchData[26 + i]);
		NetProps.SetPropVector(client, "m_hangPos", switchData[28 + i]);
		NetProps.SetPropVector(client, "m_hangStandPos", switchData[30 + i]);
		NetProps.SetPropVector(client, "m_hangNormal", switchData[32 + i]);
		NetProps.SetPropInt(client, "m_frustration", switchData[34 + i]);
		NetProps.SetPropInt(client, "m_clientIntensity", switchData[36 + i]);
		NetProps.SetPropInt(client, "m_iPlayerState", switchData[38 + i]);
		NetProps.SetPropInt(client, "pl.deadflag", switchData[40 + i]);
		NetProps.SetPropVector(client, "m_vecViewOffset", switchData[42 + i]);
		NetProps.SetPropInt(client, "m_iBonusProgress", switchData[44 + i]);
		NetProps.SetPropInt(client, "m_iBonusChallenge", switchData[46 + i]);
		NetProps.SetPropEntity(client, "m_hViewEntity", switchData[48 + i]);
		NetProps.SetPropInt(client, "m_bDucked", switchData[50 + i]);
		NetProps.SetPropInt(client, "m_bDucking", switchData[52 + i]);
		NetProps.SetPropInt(client, "m_bInDuckJump", switchData[54 + i]);
		NetProps.SetPropInt(client, "m_nDuckTimeMsecs", switchData[56 + i]);
		NetProps.SetPropInt(client, "m_nDuckJumpTimeMsecs", switchData[58 + i]);
		NetProps.SetPropInt(client, "m_nJumpTimeMsecs", switchData[60 + i]);
		NetProps.SetPropFloat(client, "m_flFallVelocity", switchData[62 + i]);
		NetProps.SetPropEntity(client, "m_hUseEntity", switchData[64 + i]);
		NetProps.SetPropEntity(client, "m_hRagdoll", switchData[66 + i]);
		NetProps.SetPropVector(client, "m_lastLadderNormal", switchData[68 + i]);
		NetProps.SetPropInt(client, "m_iShovePenalty", switchData[70 + i]);
		NetProps.SetPropFloat(client, "m_healthBuffer", switchData[72 + i]);
		NetProps.SetPropFloat(client, "m_healthBufferTime", switchData[74 + i]);
		NetProps.SetPropFloat(client, "m_itTimer.m_duration", switchData[76 + i]);
		NetProps.SetPropFloat(client, "m_itTimer.m_timestamp", switchData[78 + i]);
		NetProps.SetPropInt(client, "m_isFallingFromLedge", switchData[80 + i]);
		client.SetReviveCount(switchData[82 + i]);
		NetProps.SetPropInt(client, "m_isGoingToDie", switchData[84 + i]);
		NetProps.SetPropFloat(client, "m_vomitStart", switchData[86 + i]);
		NetProps.SetPropFloat(client, "m_vomitFadeStart", switchData[88 + i]);
		NetProps.SetPropFloat(client, "m_stunTimer.m_duration", switchData[90 + i]);
		NetProps.SetPropFloat(client, "m_stunTimer.m_timestamp", switchData[92 + i]);
		NetProps.SetPropFloat(client, "m_TimeForceExternalView", switchData[94 + i]);
		client.SetVelocity(switchData[96 + i]);
		NetProps.SetPropInt(client, "m_MoveType", switchData[98 + i]);
		NetProps.SetPropFloat(client, "m_flProgressBarDuration", 0);
		NetProps.SetPropFloat(client, "m_flProgressBarStartTime", 0);
		NetProps.SetPropEntity(client, "m_useActionOwner", null);
		NetProps.SetPropEntity(client, "m_useActionTarget", null);
		NetProps.SetPropInt(client, "m_iCurrentUseAction", 0);
		NetProps.SetPropEntity(client, "m_reviveOwner", null);
		NetProps.SetPropEntity(client, "m_reviveTarget", null);
		// Don't transfer FL_FAKECLIENT flags
		local flagData = switchData[100 + i];
		if (flagData & (1 << 8))
			flagData = flagData &~ (1 << 8);
		// TODO: FL_DUCKING (1 << 1) is problematic, persists on bots
		// Makes them slow and crouched while standing
		// But can't exactly remove it like FL_FAKECLIENT, you get stuck
		// In overhangs you crouch under
		NetProps.SetPropInt(client, "m_fFlags", flagData);

		NetProps.SetPropInt(client, "m_iEFlags", switchData[102 + i]);
		NetProps.SetPropEntity(client, "m_hGroundEntity", switchData[104 + i]);
		NetProps.SetPropVector(client, "m_Collision.m_vecMins", switchData[106 + i]);
		NetProps.SetPropVector(client, "m_Collision.m_vecMaxs", switchData[108 + i]);
		NetProps.SetPropInt(client, "cslocaldata.m_duckUntilOnGround", switchData[110 + i]);

		DoEntFire("!self", "CancelCurrentScene", "", 0, null, client);
	}
}
