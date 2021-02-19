#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "Spaceship.h"
#include "Seaship.h"
#include "IShip.h"

int64_t getShipFuel(IShip s) {
  printf("type %d\n", (int)s.type);
  printf("ptr %p\n", s.obj);

  int64_t result = 0;
  switch (s.type) {
    case IShip_Seaship: {
      Seaship* ship = (Seaship*)s.obj;
      printf("leftFuel %lld rightFuel %lld\n", ship->leftFuel, ship->rightFuel);
      result = ship->leftFuel + ship->rightFuel;
      break;
    }
    case IShip_Spaceship: {
      Spaceship* ship = (Spaceship*)s.obj;
      printf("fuel %lld\n", ship->fuel);
      result = ship->fuel;
      break;
    }
    default:
      exit(1);
  }
  free(s.obj);
  printf("result %lld\n", result);
  return result;
}
