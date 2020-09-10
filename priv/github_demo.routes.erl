#{prefix => "",
  security => false,
  routes => [
            {"/", { github_demo_main_controller, index}, #{methods => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
