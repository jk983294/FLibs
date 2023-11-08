#include <Rcpp.h>
#include <math_lm.h>
#include <zerg_file.h>
#include <zerg_fst.h>

using namespace Rcpp;

struct RcppModel : public ornate::Model {
  void load(std::string path) {
    path = ztool::FileExpandUser(path);
    if (ztool::end_with(path, "fst")) {
      ztool::FstReader reader;
      reader.read(path);
      set_n(reader.rows);
      for (auto& col : reader.cols) {
        if (col.type == 1) {
          auto& vec = *reinterpret_cast<std::vector<double>*>(col.data);
          add_feature_real(col.name, vec);
        } else if (col.type == 3) {
          // auto& vec = *reinterpret_cast<std::vector<int>*>(col.data);
          // printf("col %s type=%d data=%d\n", col.name.c_str(), col.type, vec.front());
        } else if (col.type == 5) {
          auto& vec = *reinterpret_cast<std::vector<bool>*>(col.data);
          add_feature_real(col.name, vec);
        }
      }
    } else if (ztool::end_with(path, "feather")) {
      printf("TODO file type\n");
    } else {
      printf("un support file type\n");
    }
  }
  List describe(int type) {
    if (type == 0) {
      const auto& model = ornate::Model::get_lm_model();
      return List::create(_("coef") = model.m_coefs, _("mean") = model.m_signal_mean, _("sd") = model.m_signal_sd,
        _("selected") = model.selected, _("names") = model.m_param.m_f_names, _("pvalue") = model.m_pvalues);
    } else {
      return List::create();
    }
  }
  bool train(int type, const std::vector<std::string>& features = {}) {
    return ornate::Model::train(static_cast<ornate::TrainType>(type), features);
  }
  std::vector<double> fit(int type) {
    return ornate::Model::fit(static_cast<ornate::TrainType>(type));
  }
  bool add_feature(std::string name, std::vector<double> f) {
    return ornate::Model::add_feature_real(name, f);
  }
  void set_y(std::vector<double> y){ ornate::Model::set_y_real(y); }
  void set_n(size_t n) { ornate::Model::set_n(n); }
  size_t get_n() const { return ornate::Model::get_n(); }
  void set_intercept(bool flag) { ornate::Model::set_intercept(flag); }
  bool get_intercept() const { return ornate::Model::get_intercept(); }
  void set_skip_na_feature(bool flag) { ornate::Model::set_skip_na_feature(flag); }
  bool get_skip_na_feature() const { return ornate::Model::get_skip_na_feature(); }
  void reset_untradable() { ornate::Model::reset_untradable(); }
  void set_untradable(std::string name) { ornate::Model::set_untradable(name); }
  void set_untradable_vec(const std::vector<bool>& untradable) { ornate::Model::set_untradable(untradable); }
  std::vector<std::string> get_feature_names() { return ornate::Model::get_feature_names(); }
};

RCPP_MODULE(FT) {
    Rcpp::class_<RcppModel>("Model")
        .constructor("wraps RcppModel")
        .method("set_n", &RcppModel::set_n)
        .method("get_n", &RcppModel::get_n)
        .method("set_intercept", &RcppModel::set_intercept)
        .method("get_intercept", &RcppModel::get_intercept)
        .method("set_skip_na_feature", &RcppModel::set_skip_na_feature)
        .method("get_skip_na_feature", &RcppModel::get_skip_na_feature)
        .method("reset_untradable", &RcppModel::reset_untradable)
        .method("set_untradable", &RcppModel::set_untradable)
        .method("set_y", &RcppModel::set_y)
        .method("add_feature", &RcppModel::add_feature)
        .method("set_untradable_vec", &RcppModel::set_untradable_vec)
        .method("describe", &RcppModel::describe)
        .method("train", &RcppModel::train)
        .method("fit", &RcppModel::fit);
}