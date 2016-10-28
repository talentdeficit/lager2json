defmodule Lager2JSON.Mixfile do
  use Mix.Project

  def project do
    [app: :lager2json,
     version: "1.0.1",
     language: :erlang,
     description: "a lager formatter that produces json",
     deps: deps,
     package: package]
  end

  defp deps do
    [{:jsx, "~> 2.8.0"}, {:rfc3339, "~> 0.2.0"}]
  end

  defp package do
    [name: :lager2json,
     files: ["src", "mix.exs", "rebar.config", "LICENSE-APACHE", "LICENSE-MIT", "README.md"],
     maintainers: ["@talentdeficit"],
     licenses: ["Apache 2.0", "MIT"],
     links: %{"GitHub" => "https://github.com/talentdeficit/lager2json"}]
  end
end
