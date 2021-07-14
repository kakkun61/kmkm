#ifndef PROCEDURE_H
#define PROCEDURE_H

enum procedure_unit_tag {
  procedure_unit_tag
};

struct procedure_unit {
  enum procedure_unit_tag tag;
};

struct procedure_unit const procedure_unit;

struct procedure_unit procedure_rec0(void);

struct procedure_unit procedure_rec1(void);

#endif
