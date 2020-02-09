(load-config-file '("use-package"
                    "general"
                    "appearance"
                    "navigation"
                    "evil"
                    "git"
                    "terminals"
                    "whitespace"
                    "languages/ruby"
                    "languages/go"
                    "languages/python"
                    "languages/rust"
                    "lsp")) ;; load the heaviest (and defered) package last

(provide '03-packages)
