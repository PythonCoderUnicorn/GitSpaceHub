
class Clone {
    constructor(type, field, name, iteration, quadrant){
        this.type = type
        this.field = field
        this. name = name 
        this.iteration = iteration
        this.quadrant = quadrant
    }
   
    field_ops = ['Diplomat','Scientist','Field Commander', 'Administrator']

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

    foods = ['kava nuts','rippleberries']

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

    speak(){
        console.log("The Founders are wise")
    }


}

class Vorta extends Clone{
    speak(){
        super.speak()
        console.log(" Th--------")
    }
}

let kilana = new Vorta('Vorta','Field Commander',
'Kilana','1','Gamma')

console.log(kilana.memories )






let clone1 = new Clone('Vorta','Diplomacy','Weyoun',7,'Gamma')
let clone2 = new Clone("Jem'Hadar",'soldier',"Omet'iklan",7,'Gamma')

console.log('\n')
// console.log(`${clone1.name} ${clone1.iteration} is a ${clone1.field_ops[2]}`)
// console.log(clone1.field_ops[2])
// console.log(`${clone2.name} is ${clone2.jemhadar_order[1]}`)

console.log(`${clone1.name}: ${clone1.memories}`)
// console.log(clone2 )











console.log('\n')

