STADIUM_OT_ID EQU 02000             ; entire event gives an Amnesia Psyduck (a la pokemon stadium 1)

GivePsyduck:
; Adding to the party.
	xor a ; PARTYMON
	ld [wMonType], a

; Level 15 Psyduck.
	ld a, PSYDUCK
	ld [wCurPartySpecies], a
	ld a, 15
	ld [wCurPartyLevel], a

	predef TryAddMonToParty
	jr nc, .NotGiven

; Caught data.
	ld b, CAUGHT_BY_UNKNOWN
	farcall SetGiftPartyMonCaughtData

; Holding a Gorgeous Box.
	ld bc, PARTYMON_STRUCT_LENGTH
	ld a, [wPartyCount]
	dec a
	push af
	push bc
	ld hl, wPartyMon1Item
	call AddNTimes
	ld [hl], GORGEOUS_BOX
	pop bc
	pop af

; OT ID.
	ld hl, wPartyMon1ID
	call AddNTimes
	ld a, HIGH(STADIUM_OT_ID)
	ld [hli], a
	ld [hl], LOW(STADIUM_OT_ID)
;
; Nickname.
;	ld a, [wPartyCount]
;	dec a
;	ld hl, wPartyMonNicknames
;	call SkipNames
;	ld de, SpecialShuckleNickname
;	call CopyName2

; OT.
	ld a, [wPartyCount]
	dec a
	ld hl, wPartyMonOTs
	call SkipNames
	ld de, SpecialPsyduckOT
	call CopyName2

; Engine flag for this event.
;	ld hl, wDailyFlags1
;	set DAILYFLAGS1_GOT_SHUCKIE_TODAY_F, [hl]
;	ld a, 1
;	ld [wScriptVar], a
;	ret
;
.NotGiven:
	xor a
	ld [wScriptVar], a
	ret

SpecialPsyduckOT:
	db "STADIUM@"
;
;SpecialShuckleNickname:
;	db "SHUCKIE@"
;
;ReturnShuckie:
;	farcall SelectMonFromParty
;	jr c, .refused
;
;	ld a, [wCurPartySpecies]
;	cp SHUCKLE
;	jr nz, .DontReturn
;
;	ld a, [wCurPartyMon]
;	ld hl, wPartyMon1ID
;	ld bc, PARTYMON_STRUCT_LENGTH
;	call AddNTimes

; OT ID
	ld a, [hli]
	cp HIGH(STADIUM_OT_ID)
	jr nz, .DontReturn
	ld a, [hl]
	cp LOW(STADIUM_OT_ID)
	jr nz, .DontReturn

; OT
	ld a, [wCurPartyMon]
	ld hl, wPartyMonOTs
	call SkipNames
	ld de, SpecialPsyduckOT
.CheckOT:
	ld a, [de]
	cp [hl]
	jr nz, .DontReturn
	cp "@"
	jr z, .done
	inc de
	inc hl
	jr .CheckOT

.done
;	farcall CheckCurPartyMonFainted
;	jr c, .fainted
;	ld a, [wCurPartyMon]
;	ld hl, wPartyMon1Happiness
;	ld bc, PARTYMON_STRUCT_LENGTH
;	call AddNTimes
;	ld a, [hl]
;	cp 150
;	ld a, SHUCKIE_HAPPY
;	jr nc, .HappyToStayWithYou
;	xor a ; REMOVE_PARTY
;	ld [wPokemonWithdrawDepositParameter], a
;	callfar RemoveMonFromPartyOrBox
;	ld a, SHUCKIE_RETURNED
;.HappyToStayWithYou:
	ld [wScriptVar], a
	ret

;.refused
;	ld a, SHUCKIE_REFUSED
;	ld [wScriptVar], a
;	ret
;
.DontReturn:
	xor a ; SHUCKIE_WRONG_MON
	ld [wScriptVar], a
	ret

;.fainted
;	ld a, SHUCKIE_FAINTED
;	ld [wScriptVar], a
;	ret
;										; below is code ripped from dratini special to give moves
	ld a, [wScriptVar]
	cp $2
	ret nc
	ld bc, wPartyCount
	ld a, [bc]
	ld hl, MON_SPECIES
	call .GetNthPartyMon
	ld a, [bc]
	ld c, a
	ld de, PARTYMON_STRUCT_LENGTH
.CheckForPsyduck:
; start at the end of the party and search backwards for a Seel
	ld a, [hl]
	cp PSYDUCK
	jr z, .GiveMoveset
	ld a, l
	sub e
	ld l, a
	ld a, h
	sbc d
	ld h, a
	dec c
	jr nz, .CheckForPsyduck
	ret

.GiveMoveset:
	push hl
	ld a, [wScriptVar]
	ld hl, .Movesets
	ld bc, .Moveset1 - .Moveset0
	call AddNTimes

	; get address of mon's first move
	pop de
	inc de
	inc de

.GiveMoves:
	ld a, [hl]
	and a ; is the move 00?
	ret z ; if so, we're done here

	push hl
	push de
	ld [de], a ; give the Pokémon the new move

	; get the PP of the new move
	dec a
	ld hl, Moves + MOVE_PP
	ld bc, MOVE_LENGTH
	call AddNTimes
	ld a, BANK(Moves)
	call GetFarByte

	; get the address of the move's PP and update the PP
	ld hl, MON_PP - MON_MOVES
	add hl, de
	ld [hl], a

	pop de
	pop hl
	inc de
	inc hl
	jr .GiveMoves

.Movesets:
.Moveset0:
; Psyduck does not normally learn Amnesia. This is a special gift.
	db SCRATCH
	db AMNESIA
;	db QUICK_ATTACK 			; commented out as I dont want any move
;	db THUNDER_WAVE 			; commented out as I dont want any move
	db 0
.Moveset1:
; Psyduck does not normally learn Amnesia. This is a special gift.
	db SCRATCH
	db AMNESIA
;	db QUICK_ATTACK 			; commented out as I dont want any move
;	db THUNDER_WAVE 			; commented out as I dont want any move
	db 0

.GetNthPartyMon:
; inputs:
; hl must be set to 0 before calling this function.
; a must be set to the number of Pokémon in the party.

; outputs:
; returns the address of the last Pokémon in the party in hl.
; sets carry if a is 0.

	ld de, wPartyMon1
	add hl, de
	and a
	jr z, .EmptyParty
	dec a
	ret z
	ld de, PARTYMON_STRUCT_LENGTH
.loop
	add hl, de
	dec a
	jr nz, .loop
	ret

.EmptyParty:
	scf
	ret
