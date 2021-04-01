enum myBool_tag {
  myFalse_tag,
  myTrue_tag
};
struct myBool {
  enum myBool_tag tag;
};
const struct myBool myFalse = { myFalse_tag };
const struct myBool myTrue = { myTrue_tag };
