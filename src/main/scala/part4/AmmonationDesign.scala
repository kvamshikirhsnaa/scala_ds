package part4

object AmmonationDesign extends App {

  // create a trait called Ammunition that has an abstract method, spent, that returns a Boolean
  // and a val, weight, that returns an Int

  trait Ammunition {
    def spent: Boolean
    val weight: Int
  }


  // Create a trait called RangeWeapon with an abstract Suitable Ammunition type that must be a kind of
  // Ammunition (from the trait above), a load method that takes that suitable kind
  // of ammunition and returns a Boolean
  // an abstract method shoot that returns an Optional instance of the ammunition, and another abstract method
  // weight of type Int

  trait RangeWeapon {
    type SuitableAmmunition <: Ammunition
    def load(ammo : SuitableAmmunition): Boolean
    def shoot(): Option[SuitableAmmunition]
    def weight: Int
  }

  // now create three ammunition types with the following characteristics:
  // Bullet, weight 1, spent is true if ever fired, false otherwise (implementation is up to you)

  class Bullet extends Ammunition {
    val weight = 1
    private var fired = false
    def spent = fired
    def fire: Bullet = {
      fired = true
      this
    }
    override def toString: String = s"==>"
  }

  // Bolt, weight 3, spent is always false

  class Bolt extends Ammunition {
    val weight = 3
    def spent = false

  }

  // Charge, weight 0, spent is always true

  class Charge extends Ammunition {
    val weight = 0
    def spent = true
  }

  // and three RangeWeapons:
  // A SixShooter, that can be loaded up to 6 times, can only shoot bullets, and can only shoot them while
  // it has ammo left. When out, it will return None rather than Some(Bullet). Weight should be computed as 10
  // plus the weight of the number of bullets. If the gun is full (6 bullets already), do not add the bullet and return
  // false, otherwise return true. Shooting the gun should return a spent bullet and remove it from the ammo list

  class SixShooter extends RangeWeapon {
    type SuitableAmmunition = Bullet
    private[this] var bullets = List.empty[Bullet]
    def load(bullet: Bullet): Boolean =
      if (bullets.length < 6) {
        bullets = bullet :: bullets
        true
      } else false

    def shoot(): Option[Bullet] = if (bullets.nonEmpty) {
      val bullet = bullets.head
      bullets = bullets.tail
      Some(bullet.fire)
    } else None

    val gunWeight = 10
    def weight = gunWeight + bullets.foldLeft(0)(_ + _.weight)

  }

  // a Crossbow which has Bolt as its suitable ammo, has weight of 15 plus the weight of the bolt only if loaded,
  // returns Bolt from the shoot if there is a bolt to be fired, or None otherwise, may only be loaded with one
  // Bolt at a time, and returns false if you try and load it with another Bolt. Since Bolts may be re-used, there
  // is no need to change the state of spent (if you implemented spent in the best way for Bolt, you won't be able
  // to change it anyway - hint hint).

  class Crossbow extends RangeWeapon {
    type SuitableAmmunition = Bolt
    private[this] var bolt: Option[Bolt] = None
    def load(theBolt: Bolt): Boolean =
      if (bolt.isEmpty) {
        bolt = Some(theBolt)
        true
      } else false

    def shoot() =
      if (bolt.isEmpty) None
      else {
        val theBolt = bolt
        bolt = None
        theBolt
      }

    def weight = 15 + ( if (bolt.isEmpty) 0 else bolt.get.weight )
  }

  // and a blaster, which has ammunition of Charge, always returns a weight of 5 (whether loaded or not). Any time
  // the load is called, the charger is recharged to full, and on a full charge it can shoot three shots. Shoot always
  // returns None (since there is nothing physical output from the gun firing, just energy). If you like, you can
  // print up Pew or Click to show whether the blaster fires. We will add a new method, empty, just for this type
  // so we can see when the blaster is empty. Loading the blaster with a charge, always returns true, even if it is
  // fully charged, but only allows a maximum of three shots still


  class Blaster extends RangeWeapon {
    type SuitableAmmunition = Charge

    var shots = 0
    val maxShots = 3

    def empty = shots <= 0

    def shoot() = {
      if (shots > 0) shots -= 1
      None
    }

    def load(ammo: Charge): Boolean =  {
      shots = maxShots
      true
    }

    def weight = 5

  }

    val sixShooter = new SixShooter

     println(sixShooter.shoot())//  should be (None)

     println(sixShooter.load(new Bullet))  // should be (true)
     println(sixShooter.load(new Bullet)) //  should be (true)
     println(sixShooter.load(new Bullet)) // should be (true)
     println(sixShooter.load(new Bullet))  // should be (true)
     println(sixShooter.load(new Bullet)) // should be (true)
     println(sixShooter.load(new Bullet))// should be (true)
     println(sixShooter.load(new Bullet)) // should be (false)

      // all bullets returned should be spent
     println(sixShooter.shoot().get.spent) //  should be (true)
     println(sixShooter.shoot().get.spent) // should be (true)
     println(sixShooter.shoot().get.spent) // should be (true)
     println(sixShooter.shoot().get.spent) // should be (true)
     println(sixShooter.shoot().get.spent) // should be (true)
     println(sixShooter.shoot().get.spent) // should be (true)
    // println(sixShooter.shoot().get.spent) //  NoSuchElementException: None.get


    // it ("should get heavier as you add bullets, and lighter as you shoot them") {
    // checkWeaponWeight(sixShooter, 10)
      println(sixShooter.load(new Bullet))  // should be (true)
      println(sixShooter.load(new Bullet))  //should be (true)

    //  checkWeaponWeight(sixShooter, 12)
      println(sixShooter.load(new Bullet))  // should be (true)

    //  checkWeaponWeight(sixShooter, 13)
      println(sixShooter.shoot())    // Some(==>)
      println(sixShooter.shoot())    // Some(==>)
    //  checkWeaponWeight(sixShooter, 11)


    // the following should not compile if uncommented, please make sure it doesn't
    // it ("should not allow the wrong kind of ammo") {

     //  sixShooter.load(new Bolt)
     // sixShooter.load(new Charge)


    // CROSSBOW
    val crossbow = new Crossbow

    // it ("should start empty") {
     println(crossbow.shoot()) // should be (None)


    // it ("should only load or shoot one bolt at a time")
      println(crossbow.load(new Bolt)) // should be (true)
      println(crossbow.load(new Bolt))  // should be (false)

      val theBolt = crossbow.shoot().get
      println(theBolt.spent) //  should be (false)
      println(crossbow.shoot()) //  should be (None)

      crossbow.load(theBolt) // should be (true) // re-use the same bolt, since we can
      val nextBolt = crossbow.shoot().get
      println(nextBolt.spent) // should be (false)
      println(nextBolt) // should be (theBolt)


   // it ("should get heavier when loaded and lighter when not") {
     // checkWeaponWeight(crossbow, 15)
      println(crossbow.load(new Bolt)) // should be (true)

     // checkWeaponWeight(crossbow, 18)
      println(crossbow.load(new Bolt)) // should be (false)

     // checkWeaponWeight(crossbow, 18)
      println(crossbow.shoot())

     // checkWeaponWeight(crossbow, 15)
      println(crossbow.shoot())

      //    checkWeaponWeight(crossbow, 15)


    // the following test should not compile if uncommented, please make sure it doesn't
    /* it ("should not allow the wrong kind of ammunition") {
      crossbow.load(new Bullet)
      crossbow.load(new Charge)
    } */


    // BLASTER
    val blaster = new Blaster

    // it ("should start empty") {
     println(blaster.empty)  // should be (true)


    // it ("should be able to load as many times as you like, but still only give three shots") {
      println(blaster.load(new Charge)) // should be (true)

      println(blaster.empty) // should be (false)
      println(blaster.shoot()) // should be (None)
      println(blaster.empty) // should be (false)
      println(blaster.shoot()) // should be (None)
      println(blaster.empty) // should be (false)
      println(blaster.shoot()) // should be (None)
      println(blaster.empty) // should be (true)

      println(blaster.load(new Charge)) // should be (true)
      println(blaster.load(new Charge)) // should be (true)
      println(blaster.load(new Charge)) // should be (true)
      println(blaster.load(new Charge)) //  should be (true)
      println(blaster.empty)  //  should be (false)
      println(blaster.shoot()) // should be (None)
      println(blaster.empty)  // should be (false)
      println(blaster.shoot()) // should be (None)
      println(blaster.empty)  // should be (false)
      println(blaster.shoot())  //  should be (None)
      println(blaster.empty) // should be (true)


    // it ("should not change weight depending on whether it is loaded or not") {

     println(blaster.empty) // should be (true)

      // checkWeaponWeight(blaster, 5)
     println(blaster.load(new Charge)) // should be (true)
     println( blaster.empty) // should be (false)
      // checkWeaponWeight(blaster, 5)
     println( blaster.load(new Charge)) // should be (true)
     println( blaster.empty)  // should be (false)
     // checkWeaponWeight(blaster, 5)


    // the following test should fail to compile if uncommented, please check to make sure it does
    /* it ("should not accept the wrong kind of ammo") {
      blaster.load(new Bullet)
      blaster.load(new Bolt)
    } */


}
