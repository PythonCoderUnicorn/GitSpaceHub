
/*
let federation_crew = {
        Captain: "Sisko",
        First_Officer: "Saru",
        Engineering: ["Geordi","Reno","Stamets"],
        Medical: ["Bashir", "Culber","Troi"],
        Security: ["7of9", "Worf"],
        Science: ["Hemmer", "Data", "Spock"],
        Helm: ["Nhan" , "Wesley Crusher"]
    }



console.log('\n')
Object.keys(federation_crew).forEach(key => {
    console.log(`${key} =  ${federation_crew[key]}`)
    
})
console.log('\n')

*/

class Clone {
    constructor(type, field, name, iteration, quadrant, status, rank){
        this.type = type
        this.field = field
        this. name = name 
        this.iteration = iteration
        this.quadrant = quadrant
        this.status = status
        this.rank = rank
    }

    speak() {
        console.log()
    }


}






class Vorta extends Clone{
    speak(){
        super.speak()
        console.log(`${this.name} lives to serve the Founders`)
    }

    termination() {
        return `${this.type} ${this.name} ${this.iteration} has been terminated`
    }

    field_ops = ['Diplomat', 'Scientist', 'Field Commander', 'Administrator']
    
    foods = ['kava nuts', 'rippleberries']
    
    // getter
    get memories(){
        return this.memoryLog();
    }
    
    // method
    memoryLog(){
        let i = this.iteration - 1
        let msg = ` Previous Vorta Clone ${i}'s knowledge & memory loaded`
        return  msg
    }

    physiology = {
        body:{
            skin: 'pale white',
            eyes: 'violet',
            ears: 'elogated and joined to head',
        },
        senses: {
            hearing: 'level 10',
            eyesight: 'level 4',
            taste: 'level 3'
        },
        immunology: {
            poisoning: 'protected',
            vaccinations: 'updated'
        },
        abilities: {
            telekinesis: 'not active'
        }
    }

}

let kilana = new Vorta('Vorta','Field Commander','Kilana','1','Gamma','active')
let borath = new Vorta('Vorta','Science Analyst','Borath',1,'Gamma','active')
let deyos = new Vorta('Vorta', 'Admin', 'Deyos', 1, 'Gamma', 'active')
let eris = new Vorta('Vorta', 'Spy Agent', 'Eris', 1, 'Gamma', 'active')
let gelnon = new Vorta('Vorta','Admin','Gelnon',1,'Gamma','active')
let keevan = new Vorta('Vorta', 'Field Supervisor', 'Keevan', 1, 'Gamma', 'dead 2374')
let luaran = new Vorta('Vorta', 'Field Supervisor', 'Luaran', 1, 'Gamma', 'dead 2375')
let yelgrun = new Vorta('Vorta', 'Admin', 'Yelgrun', 1, 'Gamma', 'active')
let weyoun = new Vorta('Vorta', 'Field Supervisor', 'Weyoun', 8, 'Gamma', 'active')




class JemHadar extends Clone{


    order() {
        let ranks = ['1st', '2nd', '3rd', '4th','5th','6th','7th']
        let pos = ranks[this.rank -1]
        return `${this.name} is ${pos}`
    }
    
    
    physiology = {
        body: {
            skin: 'reptilian scales',
            color: 'grey',
            max_height: 7,
            min_height: 6,
            eye_color: 'black'
        },
        senses: {
            hearing: 'level 10',
            eyesight: 'level 10',
            taste: 'level 0'
         },
        birth: {
            method: 'hatcheries',
            duration: 4,
            capabilities: {
                language: 'full',
                cognition: 'full',
                strength: 0.6
            },
            food: true
        },

        adult: {
            strength: 10,
            food: false,
            skin: 'bluish-white',
            shroud: true
        },
    }

    ketracel_white ={
        required: true,
        level: 1
    }

    the_white() {
        
        let kw = this.ketracel_white.level       
        if (kw < 1) {
            return "need for the white"
        }
        else {
            return "not time yet"
        }
    }


}







// iterate over making many unnamed clones for war
for (let i = 0; i < 10; i++){
    let soldier = new JemHadar("Jem'Hadar", 'military', 'unnamed', i, 'Gamma', 'active')
    // console.log(soldier)
 
}


// named Jem'Hadar
let amatigan = new JemHadar('', 'soldier', "Amat'igan", 1, 'Gamma', 'Active', 1)
let araktaral = new JemHadar('','soldier',"Arak'Taral",1,"Gamma", 'dead 2372 Bopak III',1)
let duranadar = new JemHadar('','soldier',"Duran'Adar",1,"Gamma", 'Active',6)
let goranagar = new JemHadar('','soldier',"Goran'Agar",1,"Gamma", 'Active',1)
let ikatika = new JemHadar('','soldier',"Ikat'ika",1,"Gamma", 'died 2373 Camp 371',1)
let ixtanarex = new JemHadar('', 'soldier', "Ixtana'Rex", 1, "Gamma", 'died 2374', 2)
let kudaketan = new JemHadar('','soldier',"Kudak'Etan",1,'Alpha','dead 2374',1)
let lamatukan = new JemHadar('','soldier',"Lamat'Ukan",1,'Alpha','Active',3)
let limarason = new JemHadar('','soldier',"Limara'Son",1,'Gamma','dead 2374',6)
let mesoclan = new JemHadar('','soldier',"Meso'Clan",1,'Gamma','dead 2372 Bopak III', 4)
let ometiklan = new JemHadar('','soldier',"Omet'iklan",1,'Gamma','Active',1)
let remataklan = new JemHadar('','soldier',"Remata'Klan",1,'Gamma','dead 2374',3)
let talaktalan = new JemHadar('','soldier',"Talak'talan",1,'Gamma','Active',3)
let temozuma = new JemHadar('', 'soldier', "Temo'Zuma", 1, 'Gamma', 'dead 2372 Bopak III', 5)
let tomantorax = new JemHadar('','soldier',"Toman'torax",1,'Gamma','dead 2372',2)
let virakkara = new JemHadar('','soldier',"Virak'kara",1,'Gamma','Active',4)
let yaktalon = new JemHadar('','soldier',"Yak'Talon",1,'Gamma','dead 2374',7)





// ===============================
console.log('\n')


// console.log(weyoun)
// console.log(kilana.termination())


// soldier1.physiology.ketracel_white = 0.4
// console.log(soldier1.ketracel_white.level = 0.3)
// console.log(soldier1.the_white())

// console.log(duranadar.order() )

// console.log(yaktalon.name, yaktalon.status)


console.log(virakkara.name, virakkara.status)



console.log('\n')

