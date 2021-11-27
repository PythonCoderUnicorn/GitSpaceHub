
# -------------------------------------
# Title: StarTrek 2
# By: Zane Dax
# Date: March 31, 2019
# Notes:
# 		this is a basic foundation for a Command Line Interface game
# 		that I wanted to finish but did not.
# -------------------------------------






#------- star trek jargon/lingo

	# https://www.reddit.com/r/DaystromInstitute/comments/514i5m/what_would_happen_if_the_founders_met_the_q/
	# https://scifi.stackexchange.com/questions/84810/were-the-dominion-aware-of-the-borg
	# http://memory-alpha.wikia.com/wiki/Changeling




"""
Stardate 45614.6 ------- season 5 ep 17
Stardate 45652.1
Stardate 45703.9

Stardate 45854.2
Stardate 45944.1
"""


systems_jargon = ['high resolution sweep','neutrino emission with no visual source','gravitational fields','electromagnetic energy',\
'photon pulses','forward phaser array','initiate computer task handoff','starboard nacelle','inertial dampeners are failing!',\
'buffer field generator','starboard manifold thrust','plasma flow','annuler confinement beam','venting drive plasma','initiate core shutdown',\
'inertial dampers failing','losing attitude control','losing antimatter containment','ejection systems off-line','core breach is emanent',\
'graviton polarimeter','distortion field is fluctuating','energy buildup in the distortion field','the core is composed of nitrium and chrondite',\
'magnetic field interference','disruptive nuclear effect within the core','transfer power conduits','dilithium chamber','particle beam',\
'detecting emission emanating from within the system','emergency bulkheads have sealed off this section','muon feedback wave in the transfer beam',\
'muon particles in the dilithium chamber','ionic interference is too heavy','activating containment field','structural integrity is at 240%',\
'gravitational field ','baryon particles','baryon sweep uses high frequency plasma field','disable the containment overrides',\
'turn the interlock off for the magnetic seal','plasma torch blew up in his hands on deck 39','radial force compensator',\
'send transient subspace signal == obscures sensors and makes pseudo core breach, interpreted as warp core breach','']


'''

'''
Commands_ = ['launch a probe','correct course 021, mark 0','activate the maneuuvering thrusters','FIRE!', \
'activate deflector dish','report to the bridge','damage report','RED ALERT','standby','maximum energy levels',\
'rechannel the navigation systems','initialize prelaunch sequence','project a particle beam','perform a narrow bandwidth analysis',\
'all hands to escape pods','take us out of warp','go to secondary systems','initalize backup generators','run a full spectrum analysis',\
'computer, analyze the','reroute to secondary generators','transfer power from warp engines to life support',\
'reconfigure hydrogen-plasma mixture to include 30% nitrium','project emotter beam heading 042.021','shields up! evasive maneuver',\
'take us within transport range','level 1 diagnostic','take all transporter systems off-line until further notice','hold position',\
'resume course','begin docking procedure','shields up','hail them','level 3 security','check the containment field','drop the force field',\
'put us in synchronous orbit','launch a class 4 probe','heading 123 mark 4','prepare transfer flow']


sensors_reply = ['no evidence found','signal is no longer working', 'no result found','life support systems','power reserves',\
'detecting an unusual reading','microfusion thrusters','systems review','thrusters not repsonding','reroute the firing sequence',\
'tractor beam','shuttle is underway','heading confirmed','rerouting the propulsion system to the transporter','sensors',\
'starboard nacelle has sustained a direct impact','eject the core','all hands abandon ship','no anomalous readings',\
'anomalous readings reported','sensor logs','unusual readings','sensors detect','on screen and magnify',\
'maneuuvering thrusters are not responding','power levels are dropping rapidly',' yellow alert','shields inoperative',\
'helm not responding','no response','engaging tractor beam','subspace scan','localized subspace scan','dekyon field sitortions',\
'warp subsystems','forward tubes armed','ready torpedos','unable to lock on with tractor beam','particle beam activated',\
'intermittant field pattern','plasma leaks','negative ion charge','primary attitudes failed','intermittant failure of the inertial dampening system',\
'inertial dampeners','energy fluctuation in this access panel','losing life support on decks 11 and 12','structural integrity is at 70%',\
'estimated breach in one minute','breach of dilithium chamber in 40 seconds','structural integrity of the dilithium chamber is at 28 percent and holding',\
'weak vital signs','cannot locate crew biologic patterns','detecting a low level nucleonic beam coming from the probe','probe is emitting unusual particle beam',\
'the beam is penetrating our shields','no response','our scanners cannot penetrate plasma distortion field','difficulty scanning object',\
'gravametric interference, preventing sensors from detecting it','aft sensors','massive EPS explosion in cargo bay 4','internal scanners have been tripped',\
'helm - has to compensate for the gravimetric interference ','detection minute tetryon particle traces in subdermal tissues ']


'''

'''

ENGINEERING_lingo = ['primary warp controllers','do a bypass patch to isolate the backup controllers','matter-antimatter injectors are failing','rerouting to secondary injector power',\
'antimatter containment system failure','setting up a subspace dampening field','data processing algorithms','total systems failure','graviton field generator is depolarized',\
'power to the primary energizing coil','exposion damaged the Romulan cloaking device','discharge of chroniton particles','chroniton particles','sensory return signal','level 3 diagnostic',\
'internal sensor sweep','lateral sensor array','modify an anyon emitter','fusion initiators','the anyon sweep neutralized the cloaking effect','set up the remote link','bandwidth limitations',\
'transport cycle is taking longer','small amount of static charge cumulation','emitter pad has 4 redundant scanners','multiplex pattern buffers','the blast wiped the ship core memory',\
'reconstitute data stream','phased matter in energy stream','half phased molecules','the confinement beam subsystems check out','phase transition coils check out',\
'the pattern buffer is working within normal range','emitter pads and targeting scanners are working','surge in the matter strean','run a scan on the Heisenberg compensators',\
'level 5 containment field','primary transporters are offline','resonance frequency scan','plasma explosion','containment field is holding at maxinum level','nucleonic particle emissions',\
'reprogram teh biofilters and screen out microbes','increase the molecular dispersion','signal resolution down to 55%','more dispersion required','increase phase transition frequency',\
'image scanners are actuating','pattern acquisition positive','reprogramming biofilter','scanners read an increase of 92 percent in mass','setting up a force field around the transport chamber',\
'patterns caught in the particle beam','the residual energy from the plasma stream amplified the charge in the buffer enough to keep your patterns from degradation',\
'rematerialization subroutine has been disabled','phase inducers are connected to emitter array','lateral array','multiphase auto-containment field','spectrographic analysis',\
'track (a ship) with their impulse ion trail','main drive assembly is down','the inducers have melted','power couplings are wrecked',\
'shunt the deuterium from the main cryo-pump to the auxiliary tank','warp grid couplers','subspace field taps','__init__ warp power transfer',\
'verifying sensor calibration','EPS mains holding stable','sensor array online','run warp power through this junction','neutrino infusion','compressed tetryon beam ',\
'field diverters','Remmler array','ODN junction box was tampered with','field diverters have to be synchronized','lazor bond a backup link','bypass the regulator',\
'main plasma flow has been shunted to the main conduit','transfer 500mg from reserve system to engine core','microcrystaline damage to the hull','shields breached by baryonic radiation','']















import os
import random as rand



def Transport_Chamber():

	destination = input(" Destination : ")

	#---------- transport sequence ------
	def engage_interlock():
		# engaging system interlock
		print(' ->[->]->[->] ')
		return buffers()

	def buffers():
		# pattern buffers synchronized
		print(' :::::::::::::')
		return phase_coils()

	def phase_coils():
		# phase transition coils
		print(' //-//-//-//-// ')
		return standby()

	def standby():
		# waiting for system clearance
		if system_clears == True:
			print(' clear ')
			return energize()
		else:
			print('[] standy for clearance []')
			return standby()

	def energize():
		# break down biological data
		# send biological data to temp data array

		def bio_breakdown():
			print(':x:|:x:|:x:|:x:|:x:|:x:|:x:')
			print(':x:|:x:|:x:|:x:|:x:|:x:|:x:')
			print(':x:|:x:|:x:|:x:|:x:|:x:|:x:')

			return bio_array()

		def bio_array():
			# temp holder of transporter bio data during transport
			print(' transport data is saved [:x:]')

	print(' transport data sent to ', destination,'\n')


def transport_error():
	error_ = randint(1,100001-1)
	
	if error_ == 100000:
		os.system('say " warning, transport error has occurred "')
		print(' ionic fluctuation in matter stream occurred')
		print(" transporter data == []")


#---------------------------------------------
def lifeSave_transportLoop():

	# import bio pattern 
	def pattern_buffer():
		print(' - - - - - - - - - - - - - - - - - - - - - - - ')
		print(' importing bio patterns into transport buffer')
		print(' - - - - - - - - - - - - - - - - - - - - - - - ')
		return buffer_lock()

	# locking the buffer then send into diagnostic cycle
	def buffer_lock():
		print(' - buffer pattern locked -')
		return diagnostic_cycle()

	
	# looping the pattern buffer, avoids degradation
	def diagnostic_cycle():
		
		print(' ::: pattern buffered ::: ')
		
	
	# send to phase inducers and back
	def phase_inducers():

		power_source = ['.','|','||','|||','||||','|||||','||||||','|||||||','|||||||||','|||||||||','||||||||||']

		for x in power_source:
			print(x)
			if x == power_source[10]:
				continue
	
		return phase_inducers()	# runs 992 times then throws an error

#---------------------------------------------


def sabotage_shuttle_remotely():

	# during flight, shoot ionic pulse beam into metaphasic matrix
	# this causes temporary system malfunction

	def start_Ion_pulse():

		# start pulse from lateral sensor array
		# __init__ pulse from the lateral sensor array or science labs 1,4,16 or bridge science stations

		print(' Create a ion pulse: 1. Confirm  2. Cancel')
		ion_pulse = int(input(' :: '))

		if ion_pulse == 1:
			print(' . .. ... .... ')
			print(' . .. ... .... ')
			print(' . .. ... .... ')
			print(' . .. ion pulse created ')

			return meta_matrix()
		else:
			exit()
	

	def meta_matrix():

		print(' -------------------')
		print(' ion pulse saved  . ')
		print(' Do you wish to send ion beam? ')

		send_beam = int(input(' 1. Confirm  2. Cancel '))
		if send_beam == 1:
			print(' input your target coordinates')
			input_coordinates = input(' :: ')

			print('| target is ', input_coordinates,' |')
			confirmation = int(input(' 1. Correct. 2. Cancel'))
			if confirmation == 1:
				print(' ')
				return fire_beam()
		else:
			exit()

	def fire_beam():
		print('\n ======================>>>> ')
		print(' ========================>>>> ')


def sabotaged_shuttle():

	# temporary system malfunction
	# internal tetryon field would be formed
	# metaphasic shields disrupted

	os.system('say " warning, computer malfunction "')
	os.system('say " helm control is temporarily offline"')
	os.system('say " warning, metaphasic shields are not available"')

	print('---------------------------')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('[ ] [ ] [ ] [ ] [ ] [ ] [ ]')
	print('---------------------------\n')


	os.system('say " warning, internal tetryon levels are increasing"')
	return meta_shields_fail()


def meta_shields_fail():

	os.system('say "warning, elevated neutrino levels detected"')
	print('\n--------------------------')
	print(' neutrino level: |||||||||  ')
	print('----------------------------')

	print('\n -------------------------')
	print(' plasma turbulence: ////    ')
	print('----------------------------')

	baryon_particles_1 = rand.randint(10, 1050)
	baryon_particles_2 = rand.randint(1051,2345)

	print('\n -------------------------')
	print(' baryon levels: ', baryon_particles_1)
	print('----------------------------')

	print('----------------------------')
	print(' baryon levels: ', baryon_particles_2)
	print('----------------------------\n')

	os.system('say " warning, toxic levels of baryon rising"')
	os.system('say " warning, unsafe neutrino levels"')
	os.system('say " life support will be depleted in 2 minutes, 30 seconds"')

	return offline_transporter()

def offline_transporter():

	print(' transporter use is not functional')
	os.system('say " solar radiation is blocking transporter use "')
	print(' transporter use is not functional')
	print(' transporter use is not functional')

	os.system('say " life support will be depleted in 2 minutes, 0 seconds"')
	os.system('say " life support will be depleted in 1 minute, 30 seconds"')
	os.system('say " life support will be depleted in 1 minute, 0 seconds"')
	os.system('say " life support will be depleted in 0 minutes, 30 seconds"')
	os.system('say " life support will be depleted in 20 seconds"')
	os.system('say " life support will be depleted in 15 seconds"')
	os.system('say " life support will be depleted in 10 seconds"')
	os.system('say " life support will be depleted in 5 seconds"')

	print('\n ^^&*(*&^%$%^&*I(*&^%$#$%^& !!!!!!!!')
	return exit()

'''
the metal is in a state of quasi-molecular flux
	inside panel junction transfers warp power to the sensor array
	subspace particle stream from junction
	composed of spatially inverted tetryon particles



auto-shutdown sequence in progress
	primary power offline
		in 1 minute
		in 30 secondds
		in 10 seconds


'''






'''



'''



warp_level = [0,0.25,0.5,0.75,1,2,3,4,5,5.7,6,6.5,7,8,9]

Rx_ = ['dexalin','vertazine, 20cc','bicaridine substitute','metorapan','dermal regeneration ']

computer_voice = []

command_replies = []

#water():
#	computer_voice ='specify water temperature'


#---------------------------------------- temporal causality loop
def ship_in_a_loop():

	# unusual readings in the helm
	print(' the helm is showing unusual readings')
	print(' -----------------------------')
	print(' #$%^%$#%^ ', '^%$#$%^&&&&^%')
	print(' -----------------------------\n')

	# distortion of space-time continuum
	os.system('say "warning, there is a distortion of the space-time continuum ahead"')

	# back away from distortion
	print(' helm console: initiate aft thrusters [1] [1] [1]\n')
	
	# maneuevering thrusters do not respond
	print(' helm console: [0] [0] [0]')
	print(' -- thrusters are offline -- ')

	# distortion field fluctuates
	os.system('say "warning, distortion field is unstable"')
	print('		--	- 	- 	-- 	-  ')
	print(' - -     -      -- - - -')
	print(' -  ---- - - --  ---   -')
	print('   -    -      --- -   -')
	print(' -- --    - ------   - -')
	print(' --   -- ---    -   -   ')
	print('    ---  -    -- -   - -')
	print(' - -     -     --  -   -')

	# power levels drop
	print('\n helm console: [ ] [ ] [ ]')
	print('___________________________________')
	print(' ships power: ||||  : |||||||||||| ')
	print('-----------------------------------\n')

	# red alert
	print(' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
	print(' ~~~~~~~~~ RED ALERT ~~~~~~~~~~~')
	print(' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')

	# energy buildup in distortion field
	print('\n helm console: energy buildup increasing >>>>')

	# a ship appears from distortion field
	print('---------------------------------------')
	print(' 									  ')
	print('		      |||||				   |||||  ')
	print('          //                    //     ')
	print('         //                    //      ')
	print('      ((    -----------     ))         ')
	print('    ((                       ))        ')
	print('    ((                       ))        ')
	print('     ((                     ))         ')
	print('       ((                 ))           ')
	print('         ((             ))             ')
	print('            ((  _____ ))               ')
	print('                                       ')
	print('---------------------------------------\n')

	#----------------------------------------------------------
	#shields go up & evasive maneuvers
	print(' helm console: shields -- [10] [10] [10] [10]')
	print(' activate: [on] [off]')
	print('                     ')



	#def evasive_manuvers():

	print(' ship gets turned >>>>>>>>>> \n')

	print(' ship ^^^^^^^^^^^^^ \n')

	print(' ship <<<<<<<<<<<< \n')


	# shields & helm are inoperative
	print(' helm console: shields -- [/] [/] [/] [/]')
	print(' activate: -- - - --')
	print(' helm control: - - - - ')

	# no response from ship
	print(' ship [ ] no response \n')


	# use of tractor beam to alter ship's trajectory
	print(' helm console: tractor beam ')
	print(' activate [on] [off]')

	print(' beam ==>==>==>>==>==>==>==>')


	# tractor beam causes damage to Enterprise's starboard nacelle
	print(' helm console: ')
	print(' damage :: starboard nacelle ')

	# fail to eject the warp core, core breach emanent
	print(' engineering console: ')
	print(' eject warp core: [no] [yes] [cancel]')
	print('--------------------------------------')
	print(' warning: failure to eject the core \n')
	os.system('say " warning, failure to eject the warp core will result in the ships destruction"')

	# all hands to escape pods
	# red alert
	print(' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
	print(' ~~~~~~~~~ RED ALERT ~~~~~~~~~~~')
	print(' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
	os.system('say " all hands to the escape pods "')
	os.system('say " all hands to the escape pods "')
	os.system('say " all hands to the escape pods "')

	# Enterprise explodes
	print(' ---> your ship blows up ')
	print('                         ')
	print('  *  .  .. * *  .    .  .')
	print(' .. .    .  * *  ... *  *')
	print('  * .  . .. ... * . . .. ')
	print(' .  . .. *  . .. . *  . .')
	print(' . .. * * * * . .. .  .  .')
	print('   *  . .    .  .   .. * .')
	print(' * .   . . *  ..     . . .')
	print(' . ..   . . *  ..  * * . .')
	print(' * .   . . *  ..     . . .')
	print(' * *  * . .. . *  ..   *  ')
	print('  .   ..  ...   .   . . . ')
	print('                          ')

	return ship_in_a_loop()

def break_loop():

	# Data uses Rikers plan
	# Data decompresses shuttlebay
	print(' helm console: shuttlebay doors')
	print(' release doors [yes] [no] [cancel]\n')

	# power is restored & clear of distortion
	print(' helm console: shuttlebay pressure <<<< ')
	print(' helm console: ships power ||||||| :: |||||||||')
	print(' helm control: [1] [1] [1] [1]\n')

	# communicate with other ship trapped in loop





''' Trilithium - trilithium resin is volatile, toxic waste byproduct of ship engines can be used as a weapon '''

'''
Data self diagnistic = warp field leaves an electromagnetic signal on his internal servo fluid system

personal quarters - deck 9, section 17
Riker's quarters 11-0912

impulse engine specifications:
	Regulation 42/15 - pressure variances on the IRC tank storage

Enterprise ship regestry number NCC-1701 

Scotty drinks green Aldebaran whisky

TNG - True Q - Amanda Rogers, honors in neurobiology, plasma dynamics, eco-regeneration

Counselor Troi changed into Romulan
	was at neuropsychology seminar @ Bokara VI
	now Major Rakal of Tal Shiar
	heading to Kelab sector
	deal with Comdr Toreth
	Corvalian freighter destroyed 'Rakal orders'

Picard's authorization code : gamma-6-0-7-3

herbal tea found in replicator files

Dr Kurak - Klingon scientist
'''










ship_in_a_loop()
break_loop()





